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

open Astring
open Lwt.Infix

module Lwt_IO = IO(Cohttp_lwt_unix_io)
open Lwt_IO

module Request = Cohttp.Request.Make(Cohttp_lwt_unix_io)
module Response = Cohttp.Response.Make(Cohttp_lwt_unix_io)

let section = Lwt_log.Section.make "websocket_lwt"
exception HTTP_Error of string

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

let with_connection ?(extra_headers = Cohttp.Header.init ())
  ?(random_string=Rng.std ?state:None) ~ctx client uri =
  let connect () =
    let module C = Cohttp in
    let nonce = random_string 16 |> B64.encode ~pad:true in
    let headers = C.Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let module Request = Cohttp.Request.Make(Cohttp_lwt_unix_io) in
    let module Response = Cohttp.Response.Make(Cohttp_lwt_unix_io) in
    let req = C.Request.make ~headers uri in
    Conduit_lwt_unix.(connect ~ctx:default_ctx client) >>= fun (flow, ic, oc) ->
    set_tcp_nodelay flow;
    let drain_handshake () =
      Request.write (fun writer -> Lwt.return_unit) req oc >>= fun () ->
      Response.read ic >>= (function
          | `Ok r -> Lwt.return r
          | `Eof -> Lwt.fail End_of_file
          | `Invalid s -> Lwt.fail @@ Failure s) >>= fun response ->
      let status = C.Response.status response in
      let headers = C.Response.headers response in
      if C.Code.(is_error @@ code_of_status status)
      then Lwt.fail @@ HTTP_Error C.Code.(string_of_status status)
      else if not (C.Response.version response = `HTTP_1_1
                   && status = `Switching_protocols
                   && CCOpt.map String.Ascii.lowercase @@
                   C.Header.get headers "upgrade" = Some "websocket"
                   && upgrade_present headers
                   && C.Header.get headers "sec-websocket-accept" =
                      Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
                  )
      then Lwt.fail (Protocol_error "Bad headers")
      else Lwt_log.info_f ~section "Connected to %s" (Uri.to_string uri)
    in
    (try%lwt
      drain_handshake ()
     with exn ->
       Lwt_io.close ic >>= fun () ->
       Lwt.fail exn)
    >>= fun () ->
    Lwt.return (ic, oc)
  in
  connect () >|= fun (ic, oc) ->
  let read_frame = make_read_frame ~random_string ~masked:true (ic, oc) in
  let read_frame () = Lwt.catch read_frame (fun exn -> Lwt.fail exn) in
  let buf = Buffer.create 128 in
  (fun () -> Lwt.catch read_frame (fun exn -> Lwt.fail exn)),
  (fun frame ->
     try%lwt
       Buffer.clear buf;
       write_frame_to_buf ~random_string ~masked:true buf frame;
       Lwt_io.write oc @@ Buffer.contents buf
     with exn -> Lwt.fail exn)

let write_failed_response oc =
  let response = Cohttp.Response.make
      ~status:`Forbidden
      ~encoding:Cohttp.Transfer.Unknown
      ()
  in
  Response.write ~flush:true begin fun writer ->
    Response.write_body writer "403 Forbidden"
  end response oc

let establish_server ?timeout ?stop ?random_string ?(exception_handler=(!Lwt.async_exception_hook)) ~ctx ~mode react =
  let module C = Cohttp in
  let id = ref @@ -1 in
  let server_fun id (ic, oc) =
    (Request.read ic >>= function
      | `Ok r -> Lwt.return r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Lwt_log.info ~section "Remote endpoint closed connection" >>= fun () ->
        Lwt.fail End_of_file
      | `Invalid reason ->
        Lwt_log.info_f ~section "Invalid input from remote endpoint: %s" reason >>= fun () ->
        Lwt.fail @@ HTTP_Error reason) >>= fun request ->
    let meth    = C.Request.meth request in
    let version = C.Request.version request in
    let headers = C.Request.headers request in
    let key = C.Header.get headers "sec-websocket-key" in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && CCOpt.map String.Ascii.lowercase @@
          C.Header.get headers "upgrade" = Some "websocket"
        && key <> None
        && upgrade_present headers
      )
    then write_failed_response oc >>= fun () -> Lwt.fail (Protocol_error "Bad headers")
    else
    let key = CCOpt.get_exn key in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers = C.Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = C.Response.make
        ~status:`Switching_protocols
        ~encoding:C.Transfer.Unknown
        ~headers:response_headers () in
    Response.write (fun writer -> Lwt.return_unit) response oc >>= fun () ->
    let buf = Buffer.create 128 in
    let send_frame fr =
      Buffer.clear buf;
      write_frame_to_buf ?random_string ~masked:false buf fr;
      Lwt_io.write oc @@ Buffer.contents buf
    in
    let read_frame = make_read_frame ?random_string ~masked:false (ic, oc) in
    react id request read_frame send_frame
  in
  Conduit_lwt_unix.serve ?timeout ?stop ~ctx ~mode
    (fun flow ic oc ->
       (try%lwt
         set_tcp_nodelay flow;
         incr id;
         server_fun !id (ic,oc)
        with
        | End_of_file ->
          Lwt_log.info ~section "Client closed connection"
        | Exit ->
          Lwt_log.info ~section "Server closed connection"
        | exn -> exception_handler exn; Lwt.return_unit
       ) [%finally Lwt_io.close ic]
    )

let mk_frame_stream recv =
  let f () =
    let%lwt fr = recv () in
    match fr.Frame.opcode with
    | Frame.Opcode.Close -> Lwt.return_none
    | _ -> Lwt.return (Some fr)
  in
  Lwt_stream.from f

let establish_standard_server ?timeout ?stop ?random_string ?exception_handler ~ctx ~mode react =
  let f id req recv send =
    let recv fr =
      let%lwt fr = recv () in
      match fr.Frame.opcode with
      | Frame.Opcode.Ping ->
          send @@ Frame.create
            ~opcode:Frame.Opcode.Pong () >>= fun () -> Lwt.return fr
      | Frame.Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          (if String.length fr.Frame.content >= 2 then
             send @@ Frame.create
               ~opcode:Frame.Opcode.Close
               ~content:(String.(sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)) ()
           else send @@ Frame.close 1000
          ) >>= fun () -> Lwt.return fr

      | _ -> Lwt.return fr
    in
    react id req recv send
  in
  establish_server ?timeout ?stop ?random_string ?exception_handler ~ctx ~mode f
