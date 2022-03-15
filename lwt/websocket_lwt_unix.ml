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

open Astring
open Websocket
open Lwt.Infix
include Websocket.Make (Cohttp_lwt_unix.IO)

let section = Lwt_log.Section.make "websocket_lwt_unix"

exception HTTP_Error of string

let http_error msg = Lwt.fail (HTTP_Error msg)
let protocol_error msg = Lwt.fail (Protocol_error msg)

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP {fd; _} -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

let fail_unless eq f = if not eq then f () else Lwt.return_unit
let fail_if eq f = if eq then f () else Lwt.return_unit

let with_connection ?(extra_headers = Cohttp.Header.init ())
    ?(random_string = Websocket.Rng.init ())
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) client uri =
  let connect () =
    let module C = Cohttp in
    let nonce = Base64.encode_exn (random_string 16) in
    let headers =
      C.Header.add_list extra_headers
        [ ("Upgrade", "websocket"); ("Connection", "Upgrade");
          ("Sec-WebSocket-Key", nonce); ("Sec-WebSocket-Version", "13") ] in
    let req = C.Request.make ~headers uri in
    Conduit_lwt_unix.connect ~ctx client
    >>= fun (flow, ic, oc) ->
    set_tcp_nodelay flow ;
    let drain_handshake () =
      Request.write (fun _writer -> Lwt.return ()) req oc
      >>= fun () ->
      Response.read ic
      >>= (function
            | `Ok r -> Lwt.return r
            | `Eof -> Lwt.fail End_of_file
            | `Invalid s -> Lwt.fail @@ Failure s )
      >>= fun response ->
      let status = C.Response.status response in
      let headers = C.Response.headers response in
      fail_if
        C.Code.(is_error @@ code_of_status status)
        (fun () -> http_error C.Code.(string_of_status status))
      >>= fun () ->
      fail_unless
        (C.Response.version response = `HTTP_1_1)
        (fun () -> protocol_error "wrong http version")
      >>= fun () ->
      fail_unless
        (status = `Switching_protocols)
        (fun () -> protocol_error "wrong status")
      >>= fun () ->
      ( match C.Header.get headers "upgrade" with
      | Some a when String.Ascii.lowercase a = "websocket" -> Lwt.return_unit
      | _ -> protocol_error "wrong upgrade" )
      >>= fun () ->
      fail_unless (upgrade_present headers) (fun () ->
          protocol_error "upgrade header not present" )
      >>= fun () ->
      ( match C.Header.get headers "sec-websocket-accept" with
      | Some accept when accept = b64_encoded_sha1sum (nonce ^ websocket_uuid)
        ->
          Lwt.return_unit
      | _ -> protocol_error "wrong accept" )
      >>= fun () ->
      Lwt_log.info_f ~section "Connected to %s" (Uri.to_string uri) in
    Lwt.catch drain_handshake (fun exn ->
        Lwt_io.close ic >>= fun () -> Lwt.fail exn )
    >>= fun () -> Lwt.return (ic, oc) in
  connect ()
  >|= fun (ic, oc) ->
  let read_frame = make_read_frame ~mode:(Client random_string) ic oc in
  let read_frame () = Lwt.catch read_frame (fun exn -> Lwt.fail exn) in
  let buf = Buffer.create 128 in
  let write_frame frame =
    Buffer.clear buf ;
    Lwt.wrap2 (write_frame_to_buf ~mode:(Client random_string)) buf frame
    >>= fun () -> Lwt_io.write oc @@ Buffer.contents buf in
  (read_frame, write_frame)

let write_failed_response oc =
  let body = "403 Forbidden" in
  let body_len = String.length body |> Int64.of_int in
  let response =
    Cohttp.Response.make ~status:`Forbidden
      ~encoding:(Cohttp.Transfer.Fixed body_len) () in
  let open Response in
  write ~flush:true (fun writer -> write_body writer body) response oc

let establish_server ?read_buf ?write_buf ?timeout ?stop
    ?(on_exn = fun exn -> !Lwt.async_exception_hook exn)
    ?(check_request = check_origin_with_host)
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ~mode react =
  let module C = Cohttp in
  let server_fun flow ic oc =
    Request.read ic
    >>= (function
          | `Ok r -> Lwt.return r
          | `Eof ->
              (* Remote endpoint closed connection. No further action necessary here. *)
              Lwt_log.info ~section "Remote endpoint closed connection"
              >>= fun () -> Lwt.fail End_of_file
          | `Invalid reason ->
              Lwt_log.info_f ~section "Invalid input from remote endpoint: %s"
                reason
              >>= fun () -> Lwt.fail @@ HTTP_Error reason )
    >>= fun request ->
    let meth = C.Request.meth request in
    let version = C.Request.version request in
    let headers = C.Request.headers request in
    let key = C.Header.get headers "sec-websocket-key" in
    ( match
        ( version,
          meth,
          C.Header.get headers "upgrade",
          key,
          upgrade_present headers,
          check_request request )
      with
    | `HTTP_1_1, `GET, Some up, Some key, true, true
      when String.Ascii.lowercase up = "websocket" ->
        Lwt.return key
    | _ ->
        write_failed_response oc
        >>= fun () -> Lwt.fail (Protocol_error "Bad headers") )
    >>= fun key ->
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers =
      C.Header.of_list
        [ ("Upgrade", "websocket"); ("Connection", "Upgrade");
          ("Sec-WebSocket-Accept", hash) ] in
    let response =
      C.Response.make ~status:`Switching_protocols ~encoding:C.Transfer.Unknown
        ~headers:response_headers () in
    Response.write (fun _writer -> Lwt.return_unit) response oc
    >>= fun () ->
    let client =
      Connected_client.create ?read_buf ?write_buf request flow ic oc in
    react client in
  Conduit_lwt_unix.serve ~on_exn ?timeout ?stop ~ctx ~mode (fun flow ic oc ->
      set_tcp_nodelay flow ;
      server_fun (Conduit_lwt_unix.endp_of_flow flow) ic oc )

let mk_frame_stream recv =
  let f () =
    recv ()
    >>= fun fr ->
    match fr.Frame.opcode with
    | Frame.Opcode.Close -> Lwt.return_none
    | _ -> Lwt.return (Some fr) in
  Lwt_stream.from f

let establish_standard_server ?read_buf ?write_buf ?timeout ?stop ?on_exn
    ?check_request ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ~mode react
    =
  let f client = react (Connected_client.make_standard client) in
  establish_server ?read_buf ?write_buf ?timeout ?stop ?on_exn ?check_request
    ~ctx ~mode f
