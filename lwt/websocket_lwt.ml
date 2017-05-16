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

module Connected_client = struct
  type t = {
    buffer: Buffer.t;
    flow: Conduit_lwt_unix.flow;
    ic: Request.IO.ic;
    oc: Request.IO.oc;
    http_request: Cohttp.Request.t;
    standard_frame_replies: bool;
    read_frame: unit -> Frame.t Lwt.t;
  }

  let create
      ?read_buf
      ?(write_buf=Buffer.create 128)
      http_request flow ic oc =
    let read_frame = make_read_frame ~mode:Server ic oc in
    {
      buffer = write_buf;
      flow;
      ic;
      oc;
      http_request;
      standard_frame_replies = false;
      read_frame;
    }

  let send { buffer; oc } frame =
    Buffer.clear buffer;
    write_frame_to_buf ~mode:Server buffer frame;
    Lwt_io.write oc @@ Buffer.contents buffer

  let standard_recv t =
    t.read_frame () >>= fun fr ->
    match fr.Frame.opcode with
    | Frame.Opcode.Ping ->
        send t @@ Frame.create
          ~opcode:Frame.Opcode.Pong () >|= fun () -> fr
    | Frame.Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.Frame.content >= 2 then
           send t @@ Frame.create
             ~opcode:Frame.Opcode.Close
             ~content:(String.(sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)) ()
         else send t @@ Frame.close 1000
        ) >|= fun () -> fr
    | _ -> Lwt.return fr

  let recv t =
    if t.standard_frame_replies then
      standard_recv t
    else
      t.read_frame ()

  let http_request { http_request; _ } = http_request

  type source =
    | TCP of Ipaddr.t * int
    | Domain_socket of string
    | Vchan of Conduit_lwt_unix.vchan_flow

  let source { flow; _ } : source =
    match flow with
    | Conduit_lwt_unix.TCP tcp_flow ->
      TCP (tcp_flow.Conduit_lwt_unix.ip, tcp_flow.Conduit_lwt_unix.port)
    | Conduit_lwt_unix.Domain_socket { path } ->
      Domain_socket path
    | Conduit_lwt_unix.Vchan flow ->
      Vchan flow

  let make_standard t = { t with standard_frame_replies = true }
end

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

let check_origin ?(origin_mandatory=false) ~hosts =
  let pred origin_host = List.exists
    (fun h -> String.Ascii.lowercase h = origin_host)
    hosts
  in
  fun request ->
    let headers = request.Cohttp.Request.headers in
    match Cohttp.Header.get headers "origin" with
    | None -> not origin_mandatory
    | Some origin ->
        let origin = Uri.of_string origin in
        match Uri.host origin with
        | None -> false
        | Some host -> (* host is already lowercased by Uri *)
            pred host

let check_origin_with_host request =
  let headers = request.Cohttp.Request.headers in
  let host = Cohttp.Header.get headers "host" in
  match host with
  | None -> failwith "Missing host header" (* mandatory in http/1.1 *)
  | Some host ->
      (* remove port *)
      let hostname = Option.value_map ~default:host ~f:fst (String.cut ~sep:":" host) in
      check_origin ~hosts:[hostname] request

let with_connection
    ?(extra_headers = Cohttp.Header.init ())
    ?(random_string=Rng.init ())
    ?(ctx=Conduit_lwt_unix.default_ctx)
    client uri =
  let connect () =
    let module C = Cohttp in
    let nonce = random_string 16 |> B64.encode ~pad:true in
    let headers = C.Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = C.Request.make ~headers uri in
    Conduit_lwt_unix.(connect ctx client) >>= fun (flow, ic, oc) ->
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
                   && Option.map ~f:String.Ascii.lowercase @@
                   C.Header.get headers "upgrade" = Some "websocket"
                   && upgrade_present headers
                   && C.Header.get headers "sec-websocket-accept" =
                      Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
                  )
      then Lwt.fail (Protocol_error "Bad headers")
      else Lwt_log.info_f ~section "Connected to %s" (Uri.to_string uri)
    in
    Lwt.catch drain_handshake begin fun exn ->
      Lwt_io.close ic >>= fun () ->
      Lwt.fail exn
    end >>= fun () ->
    Lwt.return (ic, oc)
  in
  connect () >|= fun (ic, oc) ->
  let read_frame = make_read_frame ~mode:(Client random_string) ic oc in
  let read_frame () = Lwt.catch read_frame (fun exn -> Lwt.fail exn) in
  let buf = Buffer.create 128 in
  let write_frame frame =
    Buffer.clear buf;
    Lwt.wrap2
      (write_frame_to_buf ~mode:(Client random_string)) buf frame >>= fun () ->
    Lwt_io.write oc @@ Buffer.contents buf in
  read_frame, write_frame

let write_failed_response oc =
  let body = "403 Forbidden" in
  let body_len = String.length body |> Int64.of_int in
  let response = Cohttp.Response.make
      ~status:`Forbidden
      ~encoding:(Cohttp.Transfer.Fixed body_len)
      ()
  in
  let open Response in
  write
    ~flush:true
    (fun writer -> write_body writer body)
    response oc

let establish_server
    ?read_buf ?write_buf
    ?timeout ?stop
    ?on_exn
    ?(check_request=check_origin_with_host)
    ~ctx ~mode react =
  let module C = Cohttp in
  let server_fun flow ic oc =
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
        && Option.map ~f:String.Ascii.lowercase @@
          C.Header.get headers "upgrade" = Some "websocket"
        && key <> None
        && upgrade_present headers
        && check_request request
      )
    then write_failed_response oc >>= fun () -> Lwt.fail (Protocol_error "Bad headers")
    else
    let key = Option.value_exn key in
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
    let client =
      Connected_client.create ?read_buf ?write_buf request flow ic oc in
    react client
  in
  Conduit_lwt_unix.serve ?on_exn ?timeout ?stop ~ctx ~mode begin fun flow ic oc ->
    set_tcp_nodelay flow;
    server_fun flow ic oc
  end

let mk_frame_stream recv =
  let f () =
    recv () >>= fun fr ->
    match fr.Frame.opcode with
    | Frame.Opcode.Close -> Lwt.return_none
    | _ -> Lwt.return (Some fr)
  in
  Lwt_stream.from f

let establish_standard_server
    ?read_buf ?write_buf
    ?timeout ?stop
    ?on_exn ?check_request ~ctx ~mode react =
  let f client =
    react (Connected_client.make_standard client)
  in
  establish_server ?read_buf ?write_buf ?timeout ?stop
    ?on_exn ?check_request ~ctx ~mode f
