open Core.Std
open Async.Std

include Websocket

module Async_IO = IO(Cohttp_async_io)
open Async_IO

let log = Log.create ~level:`Error ~on_error:`Raise ~output:Log.Output.([stderr ()])

exception HTTP_Error of string

module Request = Cohttp.Request.Make(Cohttp_async_io)
module Response = Cohttp.Response.Make(Cohttp_async_io)

let _ = Writer.of_pipe

let client ?(name="") ?(extra_headers = Cohttp.Header.init ())
    ~app_to_ws ~ws_to_app ~net_to_ws ~ws_to_net uri =
  let drain_handshake r w =
    let nonce = random_string ~base64:true 16 in
    let headers = Cohttp.Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Cohttp.Request.make ~headers uri in
    Request.write (fun writer -> Deferred.unit) req w >>= fun () ->
    Response.read r >>= (function
        | `Ok r -> return r
        | `Eof -> raise End_of_file
        | `Invalid s -> failwith s) >>| fun response ->
    let status = Cohttp.Response.status response in
    let headers = Cohttp.Response.headers response in
    if Cohttp.Code.(is_error @@ code_of_status status)
    then raise (HTTP_Error Cohttp.Code.(string_of_status status))
    else if not (Cohttp.Response.version response = `HTTP_1_1
                 && status = `Switching_protocols
                 && CCOpt.map String.lowercase @@
                 Cohttp.Header.get headers "upgrade" = Some "websocket"
                 && upgrade_present headers
                 && Cohttp.Header.get headers "sec-websocket-accept" =
                    Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
                )
    then failwith "Protocol error"
  in
  Nocrypto_entropy_unix.initialize (); (* FIXME: generate entropy *)
  drain_handshake net_to_ws ws_to_net >>= fun () ->
  let read_frame = make_read_frame ~masked:true (net_to_ws, ws_to_net) in
  let rec loop () =
    read_frame () >>= fun fr ->
    Pipe.write ws_to_app fr >>= fun () ->
    Log.debug log "net -> app";
    loop ()
  in
  don't_wait_for @@ loop ();
  let buf = Buffer.create 128 in
  Pipe.transfer app_to_ws Writer.(pipe ws_to_net)
    (fun fr ->
       Buffer.clear buf;
       write_frame_to_buf ~masked:true buf fr;
       Log.debug log "app -> net";
       Buffer.contents buf
    ) >>= fun () ->
  Deferred.any [Pipe.closed ws_to_app; Pipe.closed app_to_ws]

let server ?(name="") ~app_to_ws ~ws_to_app ~net_to_ws ~ws_to_net address =
  let server_fun address r w =
    (Request.read r >>| function
      | `Ok r -> r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Log.info log "Remote endpoint closed connection";
        raise End_of_file
      | `Invalid reason ->
        Log.info log "Invalid input from remote endpoint: %s" reason;
        failwith reason) >>= fun request ->
    let meth    = Cohttp.Request.meth request in
    let version = Cohttp.Request.version request in
    let headers = Cohttp.Request.headers request in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && CCOpt.map String.lowercase @@
        Cohttp.Header.get headers "upgrade" = Some "websocket"
        && upgrade_present headers
      )
    then failwith "Protocol error";
    let key = CCOpt.get_exn @@ Cohttp.Header.get headers "sec-websocket-key" in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers = Cohttp.Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = Cohttp.Response.make
        ~status:`Switching_protocols
        ~encoding:Cohttp.Transfer.Unknown
        ~headers:response_headers () in
    Response.write (fun writer -> Deferred.unit) response w
  in
  Nocrypto_entropy_unix.initialize (); (* FIXME: generate entropy *)
  Writer.of_pipe Info.(of_string "ws_to_net") ws_to_net >>= fun (w, _) ->
  Reader.of_pipe Info.(of_string "net_to_ws") net_to_ws >>= fun r ->
  server_fun address r w >>= fun () ->
  let read_frame = make_read_frame ~masked:true (r, w) in
  let rec loop () = read_frame () >>= Pipe.write ws_to_app >>= loop in
  let buf = Buffer.create 128 in
  Pipe.transfer app_to_ws Writer.(pipe w)
    (fun fr ->
       Buffer.clear buf;
       write_frame_to_buf ~masked:true buf fr;
       Buffer.contents buf
    ) >>= fun () ->
  Deferred.any [loop (); Pipe.closed ws_to_app; Pipe.closed app_to_ws]
