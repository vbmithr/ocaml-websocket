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

include Websocket

open Core
open Async
open Cohttp

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
    let rec forward_frames_to_app ws_to_app =
      read_frame () >>= fun fr ->
      begin
        if not @@ Pipe.is_closed ws_to_app then
          Pipe.write ws_to_app fr else
        Deferred.unit
      end >>= fun () ->
      forward_frames_to_app ws_to_app
    in
    let forward_frames_to_net ws_to_net app_to_ws =
      Writer.transfer ws_to_net app_to_ws begin fun fr ->
        Buffer.clear buf;
        write_frame_to_buf ~random_string ~masked:true buf fr;
        let contents = Buffer.contents buf in
        Writer.write ws_to_net contents
      end
    in
    Deferred.any_unit [
      forward_frames_to_app ws_to_app;
      forward_frames_to_net ws_to_net app_to_ws ;
      Deferred.all_unit Pipe.[closed app_to_ws ; closed ws_to_app ; ] ;
    ]
  in
  let finally_f = lazy begin
    Pipe.close_read app_to_ws ;
    Pipe.close ws_to_app
  end in
  Monitor.try_with_or_error ~name run >>| fun res ->
  Lazy.force finally_f ;
  res

let client_ez
    ?opcode
    ?log
    ?(name="client_ez")
    ?extra_headers
    ?heartbeat
    ?random_string
    uri
    _s net_to_ws ws_to_net =
  let app_to_ws, reactor_write = Pipe.create () in
  let to_reactor_write, client_write = Pipe.create () in
  let client_read, ws_to_app = Pipe.create () in
  let initialized = Ivar.create () in
  let initialized_d = Ivar.read initialized in
  let last_pong = ref @@ Time_ns.epoch in
  let cleanup = lazy begin
    Pipe.close ws_to_app ;
    Pipe.close_read app_to_ws ;
    Pipe.close_read to_reactor_write ;
    Pipe.close client_write
  end in
  let send_ping w span =
    let now = Time_ns.now () in
    Option.iter log ~f:(fun log -> Log.debug log "-> PING");
    Pipe.write w @@ Frame.create
      ~opcode:Frame.Opcode.Ping
      ~content:(Time_ns.to_string_fix_proto `Utc now) () >>| fun () ->
    let time_since_last_pong = Time_ns.diff now !last_pong in
    if !last_pong > Time_ns.epoch
    && Time_ns.Span.(time_since_last_pong > span + span) then
      Lazy.force cleanup
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
  let react () =
    initialized_d >>= fun () ->
    Pipe.transfer to_reactor_write reactor_write ~f:(fun content ->
        Frame.create ?opcode ~content ()) in
  (* Run send_ping every heartbeat when heartbeat is set. *)
  don't_wait_for begin match heartbeat with
  | None -> Deferred.unit
  | Some span -> initialized_d >>| fun () ->
    Clock_ns.run_at_intervals'
      ~continue_on_error:false
      ~stop:(Pipe.closed reactor_write)
      span (fun () -> send_ping reactor_write span)
  end ;
  don't_wait_for begin
    Monitor.protect
      ~finally:(fun () -> Lazy.force cleanup ; Deferred.unit)
      begin fun () ->
        Deferred.any_unit [
          (client ?extra_headers ?log ?random_string ~initialized
             ~app_to_ws ~ws_to_app ~net_to_ws ~ws_to_net uri |> Deferred.ignore) ;
          react () ;
          Deferred.all_unit Pipe.[ closed client_read ; closed client_write ; ]
        ]
      end
  end;
  client_read, client_write

let server
    ?log
    ?random_string
    ?(check_request = fun _ -> Deferred.return true)
    ~reader ~writer
    ~app_to_ws ~ws_to_app () =
  let handshake r w =
    (Request_async.read r >>| function
      | `Ok r -> r
      | `Eof ->
        (* Remote endpoint closed connection. No further action
           necessary here. *)
        Option.iter log ~f:begin fun log ->
          Log.info log "Remote endpoint closed connection"
        end ;
        raise End_of_file
      | `Invalid reason ->
        Option.iter log ~f:begin fun log ->
          Log.info log "Invalid input from remote endpoint: %s" reason
        end ;
        failwith reason) >>= fun request ->
    begin
      check_request request >>= function
      | true -> Deferred.unit
      | false ->
        let body = "403 Forbidden" in
        let response = Cohttp.Response.make
            ~status:`Forbidden ()
            ~encoding:(Cohttp.Transfer.Fixed (String.length body |> Int64.of_int)) in
        let open Response_async in
        write ~flush:true
          (fun w -> write_body w body)
          response w >>= fun () ->
        raise Exit
    end >>= fun () ->
    let meth    = Request.meth request in
    let version = Request.version request in
    let headers = Request.headers request in
    if not begin
        version = `HTTP_1_1
        && meth = `GET
        && Option.map (Header.get headers "upgrade") ~f:String.lowercase = Some "websocket"
        && upgrade_present headers
      end
    then failwith "Protocol error";
    let key = Option.value_exn
        ~message:"missing sec-websocket-key"
        (Header.get headers "sec-websocket-key") in
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
  Monitor.try_with_or_error
    ~extract_exn:true (fun () -> handshake reader writer) |>
  Deferred.Or_error.bind ~f:begin fun () ->
    set_tcp_nodelay writer;
    let read_frame =
      make_read_frame ?random_string ~masked:true (reader, writer) in
    let rec loop () =
      read_frame () >>=
      Pipe.write ws_to_app >>=
      loop
    in
    let transfer_end =
      let buf = Buffer.create 128 in
      Pipe.transfer app_to_ws Writer.(pipe writer) begin fun fr ->
        Buffer.clear buf;
        write_frame_to_buf ?random_string ~masked:false buf fr;
        Buffer.contents buf
      end
    in
    Monitor.protect
      ~finally:begin fun () ->
        Pipe.close ws_to_app ;
        Pipe.close_read app_to_ws ;
        Deferred.unit
      end begin fun () ->
      Deferred.any
        [transfer_end;
         loop ();
         Pipe.closed ws_to_app;
         Pipe.closed app_to_ws]
    end >>= Deferred.Or_error.return
  end
