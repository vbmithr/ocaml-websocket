open Core.Std
open Async.Std
open Cohttp

include Websocket

module Async_IO = IO(Cohttp_async_io)
open Async_IO

module Request_async = Request.Make(Cohttp_async_io)
module Response_async = Response.Make(Cohttp_async_io)

let set_tcp_nodelay writer =
  let socket = Socket.of_fd (Writer.fd writer) Socket.Type.tcp in
  Socket.setopt socket Socket.Opt.nodelay true

let maybe_debug log k = Printf.ksprintf (fun msg -> Option.iter log ~f:(fun log -> Log.debug log "%s" msg)) k
let maybe_info log k = Printf.ksprintf (fun msg -> Option.iter log ~f:(fun log -> Log.info log "%s" msg)) k
let maybe_error log k = Printf.ksprintf (fun msg -> Option.iter log ~f:(fun log -> Log.error log "%s" msg)) k

let client
    ?log
    ?(name="client")
    ?(extra_headers = Header.init ())
    ?(random_string = Rng.std ?state:None)
    ?initialized
    ~app_to_ws
    ~ws_to_app
    ~net_to_ws
    ~ws_to_net
    uri =
  let drain_handshake r w =
    let nonce = random_string 16 |> B64.encode ~pad:true in
    let headers = Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    maybe_debug log "%s" Sexp.(to_string_hum Request.(sexp_of_t req));
    Request_async.write (fun writer -> Deferred.unit) req w >>= fun () ->
    Response_async.read r >>= function
    | `Eof -> raise End_of_file
    | `Invalid s -> failwith s
    | `Ok response ->
      maybe_debug log "%s" Sexp.(to_string_hum Response.(sexp_of_t response));
      let status = Response.status response in
      let headers = Response.headers response in
      if Code.(is_error @@ code_of_status status) then
        Reader.contents r >>= fun msg ->
        maybe_error log "%s" msg;
        failwith @@ "HTTP Error " ^ Code.(string_of_status status)
      else if Response.version response <> `HTTP_1_1 then failwith "HTTP version error"
      else if status <> `Switching_protocols then failwith @@ "status error " ^ Code.(string_of_status status)
      else if Header.(get headers "upgrade") |> Option.map ~f:String.lowercase  <> Some "websocket" then failwith "upgrade error"
      else if not @@ upgrade_present headers then failwith "update not present"
      else if Header.get headers "sec-websocket-accept" <> Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum) then failwith "accept error"
      else Deferred.unit
  in
  let run () =
    drain_handshake net_to_ws ws_to_net >>= fun () ->
    Option.iter initialized (fun ivar -> Ivar.fill ivar ());
    let read_frame = make_read_frame ~random_string ~masked:true (net_to_ws, ws_to_net) in
    let buf = Buffer.create 128 in
    (* this terminates -> net_to_ws && ws_to_net is closed *)
    let rec forward_frames_to_app ws_to_app =
      if Pipe.is_closed ws_to_app then
        Deferred.unit
      else
        Monitor.try_with_or_error ~name:"Websocket.read_frame" read_frame >>= function
        | Ok fr ->
          Pipe.write ws_to_app fr >>= fun () ->
          forward_frames_to_app ws_to_app
        | Error err ->
          maybe_debug log "%s" @@ Error.to_string_hum err;
          Deferred.unit
    in
    (* ws_to_net closed <-> app_to_ws closed *)
    let forward_frames_to_net ws_to_net app_to_ws =
      Writer.transfer ws_to_net app_to_ws
        (fun fr ->
           Buffer.clear buf;
           write_frame_to_buf ~random_string ~masked:true buf fr;
           let contents = Buffer.contents buf in
           Writer.write ws_to_net contents
        )
    in
    Deferred.all_unit [
      forward_frames_to_app ws_to_app;
      forward_frames_to_net ws_to_net app_to_ws;
    ]
  in
  let finally_f () =
    Pipe.close_read app_to_ws;
    Pipe.close ws_to_app;
    Deferred.unit
  in
  Monitor.try_with_or_error ~name (fun () -> Monitor.protect ~finally:finally_f run)

let client_ez
    ?opcode
    ?log
    ?(name="client_ez")
    ?extra_headers
    ?(wait_for_pong=Time.Span.of_sec 5.)
    ?(heartbeat=Time.Span.zero)
    ?random_string
    uri
    _s r w =
  let open Frame in
  let last_pong = ref @@ Time.epoch in
  let watch w =
    after wait_for_pong >>| fun () ->
    let time_since_last_pong = Time.abs_diff !last_pong @@ Time.now () in
    if Time.Span.(time_since_last_pong > wait_for_pong)
    then Pipe.close w
  in
  let rec keepalive w =
    after heartbeat >>= fun () ->
    if Pipe.is_closed w then
      Deferred.unit
    else begin
      maybe_debug log "-> PING";
      Pipe.write w @@ Frame.create
        ~opcode:Opcode.Ping
        ~content:Time_ns.(now () |> to_string_fix_proto `Utc) () >>= fun () ->
      don't_wait_for @@ watch w;
      keepalive w
    end
  in
  let react w fr =
    maybe_debug log "<- %s" Frame.(show fr);
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
  let initialized = Ivar.create () in
  don't_wait_for begin
    Ivar.read initialized >>= fun () -> Deferred.all_unit [
      Pipe.transfer to_reactor_write reactor_write
        ~f:(fun content -> Frame.create ?opcode ~content ());
      if heartbeat <> Time.Span.zero then
        keepalive reactor_write
      else
        Deferred.unit
    ]
  end;
  let finally_f () =
    Pipe.close_read to_reactor_write;
    Pipe.close_read client_read;
    Deferred.unit
  in
  don't_wait_for begin
    Monitor.protect ~finally:finally_f
      (fun () -> client ?extra_headers ?log ?random_string ~initialized ~app_to_ws ~ws_to_app ~net_to_ws:r ~ws_to_net:w uri) |> Deferred.ignore
  end;
  client_read, client_write

let server ?log ?(name="server") ?random_string ~app_to_ws ~ws_to_app ~reader ~writer address =
  let server_fun address r w =
    (Request_async.read r >>| function
      | `Ok r -> r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        maybe_info log "Remote endpoint closed connection";
        raise End_of_file
      | `Invalid reason ->
        maybe_info log "Invalid input from remote endpoint: %s" reason;
        failwith reason) >>= fun request ->
    let meth    = Request.meth request in
    let version = Request.version request in
    let headers = Request.headers request in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && Header.get headers "upgrade" |> Option.map ~f:String.lowercase  = Some "websocket"
        && upgrade_present headers
      )
    then failwith "Protocol error";
    let key = Option.value_exn ~message:"missing sec-websocket-key" (Header.get headers "sec-websocket-key") in
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
  server_fun address reader writer >>= fun () ->
  set_tcp_nodelay writer;
  let read_frame = make_read_frame ?random_string ~masked:true (reader, writer) in
  let rec loop () = read_frame () >>= Pipe.write ws_to_app >>= loop in
  let run () =
    Monitor.try_with_or_error ~name loop >>| function
    | Ok () -> ()
    | Error err ->
      maybe_debug log "exception in server loop: %s" Error.(to_string_hum err)
  in
  let buf = Buffer.create 128 in
  let transfer_end = Pipe.transfer app_to_ws Writer.(pipe writer)
    (fun fr ->
       Buffer.clear buf;
       write_frame_to_buf ?random_string ~masked:false buf fr;
       Buffer.contents buf
    )
  in
  Deferred.any [transfer_end; run (); Pipe.closed ws_to_app; Pipe.closed app_to_ws]
