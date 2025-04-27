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
open Cohttp_lwt_unix.Private
include Websocket.Make (IO)

let section = Lwt_log.Section.make "websocket_lwt_unix"

exception HTTP_Error of string

let http_error msg = Lwt.fail (HTTP_Error msg)
let protocol_error msg = Lwt.fail (Protocol_error msg)

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

let fail_unless eq f = if not eq then f () else Lwt.return_unit
let fail_if eq f = if eq then f () else Lwt.return_unit

let drain_handshake req ic oc nonce =
  Request.write ~flush:true (fun _writer -> Lwt.return ()) req oc >>= fun () ->
  ( Response.read ic >>= function
    | `Ok r -> Lwt.return r
    | `Eof -> Lwt.fail End_of_file
    | `Invalid s -> Lwt.fail @@ Failure s )
  >>= fun response ->
  let open Cohttp in
  let status = Response.status response in
  let headers = Response.headers response in
  fail_if
    Code.(is_error @@ code_of_status status)
    (fun () -> http_error Code.(string_of_status status))
  >>= fun () ->
  fail_unless
    (Response.version response = `HTTP_1_1)
    (fun () -> protocol_error "wrong http version")
  >>= fun () ->
  fail_unless (status = `Switching_protocols) (fun () ->
      protocol_error "wrong status")
  >>= fun () ->
  (match Header.get headers "upgrade" with
  | Some a when String.Ascii.lowercase a = "websocket" -> Lwt.return_unit
  | _ -> protocol_error "wrong upgrade")
  >>= fun () ->
  fail_unless (upgrade_present headers) (fun () ->
      protocol_error "upgrade header not present")
  >>= fun () ->
  match Header.get headers "sec-websocket-accept" with
  | Some accept when accept = b64_encoded_sha1sum (nonce ^ websocket_uuid) ->
      Lwt.return_unit
  | _ -> protocol_error "wrong accept"

let connect ctx client url nonce extra_headers =
  let open Cohttp in
  let headers =
    Header.add_list extra_headers
      [
        ("Upgrade", "websocket");
        ("Connection", "Upgrade");
        ("Sec-WebSocket-Key", nonce);
        ("Sec-WebSocket-Version", "13");
      ]
  in
  let req = Request.make ~headers url in
  Conduit_lwt_unix.connect ~ctx client >>= fun (flow, ic, oc) ->
  let ic = Input_channel.create ic in
  set_tcp_nodelay flow;
  Lwt.catch
    (fun () -> drain_handshake req ic oc nonce)
    (fun exn -> Input_channel.close ic >>= fun () -> Lwt.fail exn)
  >>= fun () ->
  Lwt_log.info_f ~section "Connected to %s" (Uri.to_string url) >>= fun () ->
  Lwt.return (ic, oc)

type conn = {
  read_frame : unit -> Frame.t Lwt.t;
  write_frame : Websocket.Frame.t -> unit Lwt.t;
  oc : Lwt_io.output_channel;
}

let read { read_frame; _ } = read_frame ()
let write { write_frame; _ } frame = write_frame frame
let close_transport { oc; _ } = Lwt_io.close oc

let connect ?(extra_headers = Cohttp.Header.init ())
    ?(random_string = Websocket.Rng.init ())
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ?buf client url =
  let nonce = Base64.encode_exn (random_string 16) in
  connect ctx client url nonce extra_headers >|= fun (ic, oc) ->
  let read_frame = make_read_frame ?buf ~mode:(Client random_string) ic oc in
  let read_frame () =
    Lwt.catch read_frame (fun exn ->
        Lwt.async (fun () -> Input_channel.close ic);
        Lwt.fail exn)
  in
  let buf = Buffer.create 128 in
  let write_frame frame =
    Buffer.clear buf;
    Lwt.wrap2 (write_frame_to_buf ~mode:(Client random_string)) buf frame
    >>= fun () ->
    Lwt.catch
      (fun () ->
        Lwt_io.write oc (Buffer.contents buf) >>= fun () -> Lwt_io.flush oc)
      (fun exn ->
        Lwt.async (fun () -> Lwt_io.close oc);
        Lwt.fail exn)
  in
  { read_frame; write_frame; oc }

let write_failed_response oc =
  let body = "403 Forbidden" in
  let body_len = String.length body |> Int64.of_int in
  let response =
    Cohttp.Response.make ~status:`Forbidden
      ~encoding:(Cohttp.Transfer.Fixed body_len) ()
  in
  let open Response in
  write ~flush:true (fun writer -> write_body writer body) response oc

let server_fun ?read_buf ?write_buf check_request ic oc react =
  let read = function
    | `Ok r -> Lwt.return r
    | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Lwt_log.info ~section "Remote endpoint closed connection" >>= fun () ->
        Lwt.fail End_of_file
    | `Invalid reason ->
        Lwt_log.info_f ~section "Invalid input from remote endpoint: %s" reason
        >>= fun () -> Lwt.fail @@ HTTP_Error reason
  in
  Request.read ic >>= read >>= fun request ->
  let meth = Cohttp.Request.meth request in
  let version = Cohttp.Request.version request in
  let headers = Cohttp.Request.headers request in
  let key = Cohttp.Header.get headers "sec-websocket-key" in
  (match
     ( version,
       meth,
       Cohttp.Header.get headers "upgrade",
       key,
       upgrade_present headers,
       check_request request )
   with
  | `HTTP_1_1, `GET, Some up, Some key, true, true
    when String.Ascii.lowercase up = "websocket" ->
      Lwt.return key
  | _ ->
      write_failed_response oc >>= fun () ->
      Lwt.fail (Protocol_error "Bad headers"))
  >>= fun key ->
  let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
  let response_headers =
    Cohttp.Header.of_list
      [
        ("Upgrade", "websocket");
        ("Connection", "Upgrade");
        ("Sec-WebSocket-Accept", hash);
      ]
  in
  let response =
    Cohttp.Response.make ~status:`Switching_protocols
      ~encoding:Cohttp.Transfer.Unknown ~headers:response_headers ()
  in
  Response.write ~flush:true (fun _writer -> Lwt.return_unit) response oc
  >>= fun () ->
  let client = Connected_client.create ?read_buf ?write_buf request ic oc in
  react client

let establish_server ?read_buf ?write_buf ?timeout ?stop
    ?(on_exn = fun exn -> !Lwt.async_exception_hook exn)
    ?(check_request = check_origin_with_host)
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ~mode react =
  let module C = Cohttp in
  Conduit_lwt_unix.serve ~on_exn ?timeout ?stop ~ctx ~mode (fun flow ic oc ->
      let ic = Input_channel.create ic in
      set_tcp_nodelay flow;
      Lwt.catch
        (fun () -> server_fun ?read_buf ?write_buf check_request ic oc react)
        (function
          | End_of_file -> Lwt.return_unit
          | HTTP_Error _ -> Lwt.return_unit
          | exn -> Lwt.fail exn))

let mk_frame_stream recv =
  let f () =
    recv () >>= fun fr ->
    match fr.Frame.opcode with
    | Frame.Opcode.Close -> Lwt.return_none
    | _ -> Lwt.return (Some fr)
  in
  Lwt_stream.from f

let establish_standard_server ?read_buf ?write_buf ?timeout ?stop ?on_exn
    ?check_request ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ~mode react
    =
  let f client = react (Connected_client.make_standard client) in
  establish_server ?read_buf ?write_buf ?timeout ?stop ?on_exn ?check_request
    ~ctx ~mode f
