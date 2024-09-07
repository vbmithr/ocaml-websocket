(*
 * Copyright (c) 2012-2018 Vincent Bernardoff <vb@luminar.eu.org>
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

open Websocket
open Core
module Time_ns = Time_ns_unix
open Core.Poly
open Async
open Cohttp
module Frame = Websocket.Frame
module Async_IO = Websocket.Make (Cohttp_async.Io)
open Async_IO

let set_tcp_nodelay writer =
  let socket = Socket.of_fd (Writer.fd writer) Socket.Type.tcp in
  Socket.setopt socket Socket.Opt.nodelay true

let src =
  Logs.Src.create "websocket.async.client" ~doc:"Websocket client for Async"

let client ?(name = "websocket.client") ?(extra_headers = Header.init ())
    ?(random_string = Rng.init ()) ?initialized ~app_to_ws ~ws_to_app ~net_to_ws
    ~ws_to_net uri =
  let drain_handshake r w =
    let nonce = Base64.encode_exn (random_string 16) in
    let headers =
      Header.add_list extra_headers
        [
          ("Upgrade", "websocket");
          ("Connection", "Upgrade");
          ("Sec-WebSocket-Key", nonce);
          ("Sec-WebSocket-Version", "13");
        ]
    in
    let req = Cohttp.Request.make ~headers uri in
    Logs_async.debug ~src (fun m ->
        m "%a" Sexp.pp_hum Cohttp.Request.(sexp_of_t req))
    >>= fun () ->
    Request.write (fun _ -> Deferred.unit) req w >>= fun () ->
    Response.read r >>= function
    | `Eof -> raise End_of_file
    | `Invalid s -> failwith s
    | `Ok response ->
        Logs_async.debug ~src (fun m ->
            m "%a" Sexp.pp_hum Cohttp.Response.(sexp_of_t response))
        >>= fun () ->
        let status = Cohttp.Response.status response in
        let headers = Cohttp.Response.headers response in
        if Code.(is_error @@ code_of_status status) then
          Reader.contents r >>= fun msg ->
          Logs_async.err ~src (fun m -> m "%s" msg) >>= fun () ->
          failwith @@ "HTTP Error " ^ Code.(string_of_status status)
        else if Cohttp.Response.version response <> `HTTP_1_1 then
          failwith "HTTP version error"
        else if status <> `Switching_protocols then
          failwith @@ "status error " ^ Code.(string_of_status status)
        else if
          Header.(get headers "upgrade")
          |> Option.map ~f:String.lowercase
          <> Some "websocket"
        then failwith "upgrade error"
        else if not @@ upgrade_present headers then
          failwith "update not present"
        else if
          Header.get headers "sec-websocket-accept"
          <> Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
        then failwith "accept error"
        else Deferred.unit
  in
  let run () =
    drain_handshake net_to_ws ws_to_net >>= fun () ->
    Option.iter initialized ~f:(fun ivar -> Ivar.fill_exn ivar ());
    let read_frame =
      make_read_frame ~mode:(Client random_string) net_to_ws ws_to_net
    in
    let buf = Buffer.create 128 in
    let rec forward_frames_to_app ws_to_app =
      read_frame () >>= fun fr ->
      (if not @@ Pipe.is_closed ws_to_app then Pipe.write ws_to_app fr
      else Deferred.unit)
      >>= fun () -> forward_frames_to_app ws_to_app
    in
    let forward_frames_to_net ws_to_net app_to_ws =
      Writer.transfer' ws_to_net app_to_ws (fun frs ->
          Queue.iter frs ~f:(fun fr ->
              Buffer.clear buf;
              write_frame_to_buf ~mode:(Client random_string) buf fr;
              let contents = Buffer.contents buf in
              Writer.write ws_to_net contents);
          Writer.flushed ws_to_net)
    in
    Deferred.any_unit
      [
        forward_frames_to_app ws_to_app;
        forward_frames_to_net ws_to_net app_to_ws;
        Deferred.all_unit Pipe.[ closed app_to_ws; closed ws_to_app ];
      ]
  in
  let finally_f =
    lazy
      (Pipe.close_read app_to_ws;
       Pipe.close ws_to_app)
  in
  Monitor.try_with_or_error ~name run >>| fun res ->
  Lazy.force finally_f;
  res

let client_ez ?opcode ?(name = "websocket.client_ez") ?extra_headers ?heartbeat
    ?random_string uri net_to_ws ws_to_net =
  let app_to_ws, reactor_write = Pipe.create () in
  let to_reactor_write, client_write = Pipe.create () in
  let client_read, ws_to_app = Pipe.create () in
  let initialized = Ivar.create () in
  let initialized_d = Ivar.read initialized in
  let last_pong = ref @@ Time_ns.epoch in
  let cleanup =
    lazy
      (Pipe.close ws_to_app;
       Pipe.close_read app_to_ws;
       Pipe.close_read to_reactor_write;
       Pipe.close client_write)
  in
  let send_ping w span =
    let now = Time_ns.now () in
    Logs_async.debug ~src (fun m -> m "-> PING") >>= fun () ->
    Pipe.write w
    @@ Frame.create ~opcode:Frame.Opcode.Ping
         ~content:(Time_ns.to_string_fix_proto `Utc now)
         ()
    >>| fun () ->
    let time_since_last_pong = Time_ns.diff now !last_pong in
    if
      !last_pong > Time_ns.epoch
      && Time_ns.Span.(time_since_last_pong > span + span)
    then Lazy.force cleanup
  in
  let react w fr =
    let open Frame in
    Logs_async.debug ~src (fun m -> m "<- %a" Frame.pp fr) >>= fun () ->
    match fr.opcode with
    | Opcode.Ping ->
        Pipe.write w @@ Frame.create ~opcode:Opcode.Pong () >>| fun () -> None
    | Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.content >= 2 then
         Pipe.write w
         @@ Frame.create ~opcode:Opcode.Close
              ~content:(String.sub fr.content ~pos:0 ~len:2)
              ()
        else Pipe.write w @@ Frame.close 1000)
        >>| fun () ->
        Pipe.close w;
        None
    | Opcode.Pong ->
        last_pong := Time_ns.now ();
        return None
    | Opcode.Text | Opcode.Binary -> return @@ Some fr.content
    | _ ->
        Pipe.write w @@ Frame.close 1002 >>| fun () ->
        Pipe.close w;
        None
  in
  let client_read = Pipe.filter_map' client_read ~f:(react reactor_write) in
  let react () =
    initialized_d >>= fun () ->
    Pipe.transfer to_reactor_write reactor_write ~f:(fun content ->
        Frame.create ?opcode ~content ())
  in
  (* Run send_ping every heartbeat when heartbeat is set. *)
  don't_wait_for
    (match heartbeat with
    | None -> Deferred.unit
    | Some span ->
        initialized_d >>| fun () ->
        Clock_ns.run_at_intervals' ~continue_on_error:false
          ~stop:(Pipe.closed reactor_write) span (fun () ->
            send_ping reactor_write span));
  don't_wait_for
    (Monitor.protect
       ~finally:(fun () ->
         Lazy.force cleanup;
         Deferred.unit)
       (fun () ->
         Deferred.any_unit
           [
             client ~name ?extra_headers ?random_string ~initialized ~app_to_ws
               ~ws_to_app ~net_to_ws ~ws_to_net uri
             |> Deferred.ignore_m;
             react ();
             Deferred.all_unit Pipe.[ closed client_read; closed client_write ];
           ]));
  (client_read, client_write)

let src =
  Logs.Src.create "websocket.async.server" ~doc:"Websocket server for Async"

let server ?(name = "websocket.server")
    ?(check_request = fun _ -> Deferred.return true)
    ?(select_protocol = fun _ -> None) ~reader ~writer ~app_to_ws ~ws_to_app ()
    =
  let handshake r w =
    (Request.read r >>= function
     | `Ok r -> Deferred.return r
     | `Eof ->
         (* Remote endpoint closed connection. No further action
              necessary here. *)
         Logs_async.info ~src (fun m -> m "Remote endpoint closed connection")
         >>= fun () -> raise End_of_file
     | `Invalid reason ->
         Logs_async.info ~src (fun m ->
             m "Invalid input from remote endpoint: %s" reason)
         >>= fun () -> failwith reason)
    >>= fun request ->
    (check_request request >>= function
     | true -> Deferred.unit
     | false ->
         let body = "403 Forbidden" in
         let response =
           Cohttp.Response.make ~status:`Forbidden ()
             ~encoding:
               (Cohttp.Transfer.Fixed (String.length body |> Int64.of_int))
         in
         let open Response in
         write ~flush:true (fun w -> write_body w body) response w >>= fun () ->
         raise Exit)
    >>= fun () ->
    let meth = Cohttp.Request.meth request in
    let version = Cohttp.Request.version request in
    let headers = Cohttp.Request.headers request in
    if
      not
        (version = `HTTP_1_1 && meth = `GET
        && Option.map (Header.get headers "upgrade") ~f:String.lowercase
           = Some "websocket"
        && upgrade_present headers)
    then failwith "Protocol error";
    let key =
      Option.value_exn ~message:"missing sec-websocket-key"
        (Header.get headers "sec-websocket-key")
    in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let subprotocol =
      Option.value_map (Header.get headers "sec-websocket-protocol") ~default:[]
        ~f:(fun p ->
          Option.value_map (select_protocol p) ~default:[] ~f:(fun selected ->
              [ ("Sec-WebSocket-Protocol", selected) ]))
    in
    let response_headers =
      ("Upgrade", "websocket") :: ("Connection", "Upgrade")
      :: ("Sec-WebSocket-Accept", hash)
      :: subprotocol
    in
    let response =
      Cohttp.Response.make ~status:`Switching_protocols
        ~encoding:Transfer.Unknown
        ~headers:(Header.of_list response_headers)
        ()
    in
    Response.write (fun _ -> Deferred.unit) response w
  in
  Monitor.try_with_or_error ~name ~extract_exn:true (fun () ->
      handshake reader writer)
  |> Deferred.Or_error.bind ~f:(fun () ->
         set_tcp_nodelay writer;
         let read_frame = make_read_frame ~mode:Server reader writer in
         let rec loop () = read_frame () >>= Pipe.write ws_to_app >>= loop in
         let transfer_end =
           let buf = Buffer.create 128 in
           Pipe.transfer app_to_ws
             Writer.(pipe writer)
             ~f:(fun fr ->
               Buffer.clear buf;
               write_frame_to_buf ~mode:Server buf fr;
               Buffer.contents buf)
         in
         Monitor.protect
           ~finally:(fun () ->
             Pipe.close ws_to_app;
             Pipe.close_read app_to_ws;
             Deferred.unit)
           (fun () ->
             Deferred.any
               [
                 transfer_end;
                 loop ();
                 Pipe.closed ws_to_app;
                 Pipe.closed app_to_ws;
               ])
         >>= Deferred.Or_error.return)

let upgrade_connection ?(select_protocol = fun _ -> None)
    ?(ping_interval = Time_ns.Span.of_int_sec 50) ~app_to_ws ~ws_to_app ~f
    request =
  let headers = Cohttp.Request.headers request in
  let key =
    Option.value_exn ~message:"missing sec-websocket-key"
      (Header.get headers "sec-websocket-key")
  in
  let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
  let subprotocol =
    Option.value_map (Header.get headers "sec-websocket-protocol") ~default:[]
      ~f:(fun p ->
        Option.value_map (select_protocol p) ~default:[] ~f:(fun selected ->
            [ ("Sec-WebSocket-Protocol", selected) ]))
  in
  let response_headers =
    ("Upgrade", "websocket") :: ("Connection", "Upgrade")
    :: ("Sec-WebSocket-Accept", hash)
    :: subprotocol
  in
  let response =
    Cohttp.Response.make ~status:`Switching_protocols ~encoding:Transfer.Unknown
      ~headers:(Header.of_list response_headers)
      ()
  in
  let handler reader writer =
    let read_frame = make_read_frame ~mode:Server reader writer in
    let rec loop () =
      try_with read_frame >>= function
      | Error _ -> Deferred.unit
      | Ok frame -> Pipe.write ws_to_app frame >>= loop
    in
    let buf = Buffer.create 128 in
    let frame_to_string fr =
      Buffer.clear buf;
      write_frame_to_buf ~mode:Server buf fr;
      Buffer.contents buf
    in
    let ping () =
      if Time_ns.Span.(ping_interval = zero) then Deferred.never ()
      else
        let ping_frame_string =
          frame_to_string Frame.(create ~opcode:Opcode.Ping ())
        in
        let rec ping_loop () =
          Clock_ns.after ping_interval >>= fun () ->
          match Writer.is_closed writer with
          | true -> Deferred.unit
          | false ->
              Writer.write writer ping_frame_string;
              Writer.flushed writer >>= fun () -> ping_loop ()
        in
        ping_loop ()
    in
    let transfer_end () =
      Pipe.transfer app_to_ws Writer.(pipe writer) ~f:frame_to_string
    in
    let finally () =
      Pipe.close ws_to_app;
      Pipe.close_read app_to_ws;
      Deferred.unit
    in
    Monitor.protect ~finally (fun () ->
        set_tcp_nodelay writer;
        Deferred.all_unit
          [
            Deferred.any
              [
                transfer_end ();
                loop ();
                ping ();
                Pipe.closed ws_to_app;
                Pipe.closed app_to_ws;
              ];
            f ();
          ])
  in
  (response, handler)
