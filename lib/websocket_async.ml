open Core.Std
open Async.Std
open Cohttp

include Websocket

module Async_IO = IO(Cohttp_async_io)
open Async_IO

module Request_async = Request.Make(Cohttp_async_io)
module Response_async = Response.Make(Cohttp_async_io)

let debug log =
  Printf.ksprintf
    (fun msg -> Option.iter log ~f:(fun log -> Log.debug log "%s" msg))

let info log =
  Printf.ksprintf
    (fun msg -> Option.iter log ~f:(fun log -> Log.info log "%s" msg))

let error log =
  Printf.ksprintf
    (fun msg -> Option.iter log ~f:(fun log -> Log.error log "%s" msg))

let client
    ?log
    ?(name="")
    ?(extra_headers = Header.init ())
    ~g
    ~app_to_ws
    ~ws_to_app
    ~net_to_ws
    ~ws_to_net
    uri =
  let drain_handshake r w =
    let nonce = random_string ~g ~base64:true 16 in
    let headers = Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    Request_async.write (fun writer -> Deferred.unit) req w >>= fun () ->
    Response_async.read r >>| function
    | `Eof -> raise End_of_file
    | `Invalid s -> failwith s
    | `Ok response ->
        let status = Response.status response in
        let headers = Response.headers response in
        if Code.(is_error @@ code_of_status status) then failwith @@ "HTTP Error " ^ Code.(string_of_status status)
        else if Response.version response <> `HTTP_1_1 then failwith "HTTP version error"
        else if status <> `Switching_protocols then failwith @@ "status error " ^ Code.(string_of_status status)
        else if CCOpt.map String.lowercase Header.(get headers "upgrade") <> Some "websocket" then failwith "upgrade error"
        else if not @@ upgrade_present headers then failwith "update not present"
        else if Header.get headers "sec-websocket-accept" <> Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum) then failwith "accept error"
        else ()
  in
  let run () =
    drain_handshake net_to_ws ws_to_net >>= fun () ->
    let read_frame = make_read_frame ~masked:true (net_to_ws, ws_to_net) in
    let buf = Buffer.create 128 in
    (* this terminates -> net_to_ws && ws_to_net is closed *)
    let rec forward_frames_to_app () =
      try_with
        (fun () -> read_frame ~g () >>= function
           | `Error msg -> failwith msg
           | `Ok fr ->
             Pipe.write ws_to_app fr >>| fun () ->
             debug log "net -> app (%d bytes)" String.(length fr.Frame.content);
        ) >>= function
      | Ok () -> forward_frames_to_app ()
      | Error exn ->
        debug log "%s" Exn.(to_string exn);
        Deferred.unit
    in
    (* ws_to_net closed <-> app_to_ws closed *)
    let forward_frames_to_net () =
      Writer.transfer ws_to_net app_to_ws
        (fun fr ->
           Buffer.clear buf;
           write_frame_to_buf ~g ~masked:true buf fr;
           let contents = Buffer.contents buf in
           debug log "app -> net: %S" contents;
           Writer.write ws_to_net contents
        )
    in
    Deferred.any [
      forward_frames_to_app ();
      forward_frames_to_net ();
      Pipe.closed app_to_ws; (* The user wants to close *)
    ]
  in
  don't_wait_for begin
    try_with ~name:"client" run >>| function
    | Ok () -> ()
    | Error exn ->
      error log "%s" Exn.(to_string exn);
      Pipe.close_read app_to_ws;
      Pipe.close ws_to_app;
  end

let client_ez
    ?log
    ?(wait_for_pong=Time.Span.of_sec 5.)
    ?(heartbeat=Time.Span.zero)
    ~g
    uri
    _s r w =
  let open Frame in
  let last_pong = ref @@ Time.epoch in
  let rec keepalive w =
    let rec watch () =
      after wait_for_pong >>| fun () ->
      let time_since_last_pong = Time.abs_diff !last_pong @@ Time.now () in
      if Time.Span.(time_since_last_pong > wait_for_pong)
      then Pipe.close w
    in
    after heartbeat >>= fun () ->
    Pipe.write w @@ Frame.create
      ~opcode:Opcode.Ping ~content:Time.(now () |> to_string) () >>= fun () ->
    debug log "-> PING";
    don't_wait_for @@ watch ();
    keepalive w
  in
  let react w fr =
    debug log "<- %s" Frame.(show fr);
    match fr.opcode with
    | Opcode.Ping ->
        Pipe.write w @@ Frame.create ~opcode:Opcode.Pong () >>| fun () ->
        None
    | Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.content >= 2 then
           Pipe.write w @@ Frame.create ~opcode:Opcode.Close
             ~content:(String.sub fr.content 0 2) ()
         else Pipe.write w @@ Frame.close 1000) >>| fun () ->
        Pipe.close w;
        None
    | Opcode.Pong ->
        last_pong := Time.now (); return None
    | Opcode.Text | Opcode.Binary ->
        return @@ Some fr.content
    | _ ->
        Pipe.write w @@ Frame.close 1002 >>| fun () -> Pipe.close w; None
  in
  let app_to_ws, reactor_write = Pipe.create () in
  let to_reactor_write, client_write = Pipe.create () in
  let client_read, ws_to_app = Pipe.create () in
  let client_read = Pipe.filter_map' client_read ~f:(react reactor_write) in
  let run () =
    Deferred.all_unit [
      Pipe.transfer to_reactor_write reactor_write
        ~f:(fun content -> Frame.create ~content ());
      if heartbeat <> Time.Span.zero then
        keepalive reactor_write else
        Deferred.never ()
    ]
  in
  client ?log ~g ~app_to_ws ~ws_to_app ~net_to_ws:r ~ws_to_net:w uri;
  don't_wait_for begin
    try_with ~name:"client_ez" run >>| function
    | Ok () -> ()
    | Error exn ->
      error log "%s" Exn.(to_string exn);
      Pipe.close reactor_write;
      Pipe.close_read to_reactor_write;
      Pipe.close client_write;
      Pipe.close_read client_read
  end;
  client_read, client_write

let server ?log ?(name="") ~g ~app_to_ws ~ws_to_app ~net_to_ws ~ws_to_net address =
  let server_fun address r w =
    (Request_async.read r >>| function
      | `Ok r -> r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        info log "Remote endpoint closed connection";
        raise End_of_file
      | `Invalid reason ->
        info log "Invalid input from remote endpoint: %s" reason;
        failwith reason) >>= fun request ->
    let meth    = Request.meth request in
    let version = Request.version request in
    let headers = Request.headers request in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && CCOpt.map String.lowercase @@
        Header.get headers "upgrade" = Some "websocket"
        && upgrade_present headers
      )
    then failwith "Protocol error";
    let key = CCOpt.get_exn @@ Header.get headers "sec-websocket-key" in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers = Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = Response.make
        ~status:`Switching_protocols
        ~encoding:Transfer.Unknown
        ~headers:response_headers () in
    Response_async.write (fun writer -> Deferred.unit) response w
  in
  Writer.of_pipe Info.(of_string "ws_to_net") ws_to_net >>= fun (w, _) ->
  Reader.of_pipe Info.(of_string "net_to_ws") net_to_ws >>= fun r ->
  server_fun address r w >>= fun () ->
  let read_frame = make_read_frame ~masked:true (r, w) in
  let run () =
    read_frame ~g () >>= function
    | `Error msg -> failwith msg
    | `Ok fr -> Pipe.write ws_to_app fr
  in
  let rec loop () =
    try_with ~name:"server" run >>= function
    | Ok () -> loop ()
    | Error exn ->
      debug log "%s" Exn.(to_string exn);
      loop ()
  in
  let buf = Buffer.create 128 in
  let transfer_end = Pipe.transfer app_to_ws Writer.(pipe w)
    (fun fr ->
       Buffer.clear buf;
       write_frame_to_buf ~g ~masked:true buf fr;
       Buffer.contents buf
    )
  in
  Deferred.any [transfer_end; loop (); Pipe.closed ws_to_app; Pipe.closed app_to_ws]
