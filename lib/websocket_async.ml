(*
 * Copyright (c) 2012-2016 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

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
    Option.iter log ~f:(fun log -> Log.debug log "%s" Sexp.(to_string_hum Request.(sexp_of_t req)));
    Request_async.write (fun writer -> Deferred.unit) req w >>= fun () ->
    Response_async.read r >>= function
    | `Eof -> raise End_of_file
    | `Invalid s -> failwith s
    | `Ok response ->
        Option.iter log ~f:(fun log -> Log.debug log "%s" Sexp.(to_string_hum Response.(sexp_of_t response)));
      let status = Response.status response in
      let headers = Response.headers response in
      if Code.(is_error @@ code_of_status status) then
        Reader.contents r >>= fun msg ->
        Option.iter log ~f:(fun log -> Log.error log "%s" msg);
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
          Option.iter log ~f:(fun log -> Log.error log "%s" @@ Error.to_string_hum err);
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
    ?(wait_for_pong=Time_ns.Span.of_int_sec 5)
    ?(heartbeat=Time_ns.Span.zero)
    ?random_string
    uri
    _s r w =
  let app_to_ws, reactor_write = Pipe.create () in
  let to_reactor_write, client_write = Pipe.create () in
  let client_read, ws_to_app = Pipe.create () in
  let initialized = Ivar.create () in
  let last_pong = ref @@ Time_ns.epoch in
  let finally_f () =
    Pipe.close_read to_reactor_write;
    Pipe.close_read client_read;
    Deferred.all_unit [Reader.close r; Writer.close w]
  in
  let watch w =
    Clock_ns.after wait_for_pong >>= fun () ->
    let time_since_last_pong = Time_ns.abs_diff !last_pong @@ Time_ns.now () in
    if Time_ns.Span.(time_since_last_pong > wait_for_pong + of_int_sec 5)
    then finally_f ()
    else Deferred.unit
  in
  let rec keepalive w =
    Clock_ns.after heartbeat >>= fun () ->
    if Pipe.is_closed w then
      Deferred.unit
    else begin
      Option.iter log ~f:(fun log -> Log.debug log "-> PING");
      Pipe.write w @@ Frame.create
        ~opcode:Frame.Opcode.Ping
        ~content:Time_ns.(now () |> to_string_fix_proto `Utc) () >>= fun () ->
      don't_wait_for @@ watch w;
      keepalive w
    end
  in
  let react w fr =
    let open Frame in
    Option.iter log ~f:(fun log -> Log.debug log "<- %s" Frame.(show fr));
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
        last_pong := Time_ns.now (); return None
    | Opcode.Text | Opcode.Binary ->
        return @@ Some fr.content
    | _ ->
        Pipe.write w @@ Frame.close 1002 >>| fun () -> Pipe.close w; None
  in
  let client_read = Pipe.filter_map' client_read ~f:(react reactor_write) in
  don't_wait_for begin
    Ivar.read initialized >>= fun () -> Deferred.all_unit [
      Pipe.transfer to_reactor_write reactor_write
        ~f:(fun content -> Frame.create ?opcode ~content ());
      if heartbeat <> Time_ns.Span.zero then
        keepalive reactor_write
      else
        Deferred.unit
    ]
  end;
  don't_wait_for begin
    Monitor.protect ~finally:finally_f begin fun () ->
      client ?extra_headers ?log ?random_string ~initialized
        ~app_to_ws ~ws_to_app ~net_to_ws:r ~ws_to_net:w uri
    end |> Deferred.ignore
  end;
  client_read, client_write

let server ?log ?(name="server") ?random_string ~app_to_ws ~ws_to_app ~reader ~writer address =
  let server_fun address r w =
    (Request_async.read r >>| function
      | `Ok r -> r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Option.iter log ~f:(fun log -> Log.info log "Remote endpoint closed connection");
        raise End_of_file
      | `Invalid reason ->
        Option.iter log ~f:(fun log -> Log.info log "Invalid input from remote endpoint: %s" reason);
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
      Option.iter log ~f:(fun log -> Log.error log "exception in server loop: %s" Error.(to_string_hum err))
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
