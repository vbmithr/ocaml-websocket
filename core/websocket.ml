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

let b64_encoded_sha1sum s = Base64.encode_exn (Sha1.sha_1 s)
let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

module Rng = struct
  let init () len = Mirage_crypto_rng.generate len
end

module Frame = struct
  module Opcode = struct
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int

    let to_string = function
      | Continuation -> "continuation"
      | Text -> "text"
      | Binary -> "binary"
      | Close -> "close"
      | Ping -> "ping"
      | Pong -> "pong"
      | Ctrl i -> "ctrl " ^ string_of_int i
      | Nonctrl i -> "nonctrl " ^ string_of_int i

    let pp ppf t = Format.fprintf ppf "%s" (to_string t)

    let of_enum = function
      | i when i < 0 || i > 0xf -> invalid_arg "Frame.Opcode.of_enum"
      | 0 -> Continuation
      | 1 -> Text
      | 2 -> Binary
      | 8 -> Close
      | 9 -> Ping
      | 10 -> Pong
      | i when i < 8 -> Nonctrl i
      | i -> Ctrl i

    let to_enum = function
      | Continuation -> 0
      | Text -> 1
      | Binary -> 2
      | Close -> 8
      | Ping -> 9
      | Pong -> 10
      | Ctrl i -> i
      | Nonctrl i -> i

    let is_ctrl opcode = to_enum opcode > 7
  end

  type t = {
    opcode : Opcode.t;
    extension : int;
    final : bool;
    content : string;
  }

  let pp ppf { opcode; extension; final; content } =
    Format.fprintf ppf "[%a (0x%x) (final=%b) %s]" Opcode.pp opcode extension
      final content

  let show t = Format.asprintf "%a" pp t

  let create ?(opcode = Opcode.Text) ?(extension = 0) ?(final = true)
      ?(content = "") () =
    { opcode; extension; final; content }

  let of_bytes ?opcode ?extension ?final content =
    let content = Bytes.to_string content in
    create ?opcode ?extension ?final ~content ()

  let close code =
    let content = Bytes.create 2 in
    EndianBytes.BigEndian.set_int16 content 0 code;
    of_bytes ~opcode:Opcode.Close content
end

let xor mask msg =
  for i = 0 to Bytes.length msg - 1 do
    (* masking msg to send *)
    Bytes.set msg i
      Char.(to_int mask.[i mod 4] lxor to_int (Bytes.get msg i) |> of_byte)
  done

let is_bit_set idx v = (v lsr idx) land 1 = 1
let set_bit v idx b = if b then v lor (1 lsl idx) else v land lnot (1 lsl idx)
let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

let upgrade_present hs =
  Cohttp.Header.get_multi hs "connection" |> fun hs ->
  List.map (String.cuts ~sep:",") hs |> fun hs ->
  List.flatten hs |> fun hs ->
  List.map String.(fun h -> h |> String.Ascii.lowercase |> trim) hs
  |> List.mem "upgrade"

exception Protocol_error of string

let proto_error x = Format.kasprintf (fun x -> raise (Protocol_error x)) x

let check_origin ?(origin_mandatory = false) ~hosts =
  let pred origin_host =
    List.exists (fun h -> String.Ascii.lowercase h = origin_host) hosts
  in
  fun request ->
    let headers = request.Cohttp.Request.headers in
    match Cohttp.Header.get headers "origin" with
    | None -> not origin_mandatory
    | Some origin -> (
        let origin = Uri.of_string origin in
        match Uri.host origin with
        | None -> false
        | Some host ->
            (* host is already lowercased by Uri *)
            pred host)

let check_origin_with_host request =
  let headers = request.Cohttp.Request.headers in
  let host = Cohttp.Header.get headers "host" in
  match host with
  | None -> failwith "Missing host header" (* mandatory in http/1.1 *)
  | Some host ->
      (* remove port *)
      let hostname =
        match String.cut ~sep:":" host with None -> host | Some (h, _) -> h
      in
      check_origin ~hosts:[ hostname ] request

module type S = sig
  module IO : Cohttp.S.IO

  type mode = Client of (int -> string) | Server

  val make_read_frame :
    ?max_len:int ->
    ?buf:Buffer.t -> mode:mode -> IO.ic -> IO.oc -> unit -> Frame.t IO.t

  val write_frame_to_buf : mode:mode -> Buffer.t -> Frame.t -> unit

  module Request :
    Cohttp.S.Http_io
      with type t = Cohttp.Request.t
       and type 'a IO.t = 'a IO.t
       and type IO.ic = IO.ic
       and type IO.oc = IO.oc

  module Response :
    Cohttp.S.Http_io
      with type t = Cohttp.Response.t
       and type 'a IO.t = 'a IO.t
       and type IO.ic = IO.ic
       and type IO.oc = IO.oc

  module Connected_client : sig
    type t

    val create :
      ?max_len:int ->
      ?read_buf:Buffer.t ->
      ?write_buf:Buffer.t ->
      Cohttp.Request.t ->
      Conduit.endp ->
      IO.ic ->
      IO.oc ->
      t

    val make_standard : t -> t
    val send : t -> Frame.t -> unit IO.t
    val send_multiple : t -> Frame.t list -> unit IO.t
    val recv : t -> Frame.t IO.t
    val http_request : t -> Cohttp.Request.t
    val source : t -> Conduit.endp
  end
end

module Make (IO : Cohttp.S.IO) = struct
  open IO
  module IO = IO

  type mode = Client of (int -> string) | Server

  let is_client mode = mode <> Server

  let rec read_exactly ic remaining buf =
    read ic remaining >>= function
    | "" -> return None
    | s ->
        let recv_len = String.length s in
        Buffer.add_string buf s;
        if remaining - recv_len <= 0 then return @@ Some (Buffer.contents buf)
        else read_exactly ic (remaining - recv_len) buf

  let read_uint16 ic buf =
    read_exactly ic 2 buf >>= fun s ->
    match s with
    | None -> return None
    | Some s -> return @@ Some (EndianString.BigEndian.get_uint16 s 0)

  let read_int64 ic buf =
    read_exactly ic 8 buf >>= fun s ->
    match s with
    | None -> return None
    | Some s ->
        return @@ Some (Int64.to_int @@ EndianString.BigEndian.get_int64 s 0)

  let write_frame_to_buf ~mode buf fr =
    let scratch = Bytes.create 8 in
    let open Frame in
    let content = Bytes.of_string fr.content in
    let len = Bytes.length content in
    let opcode = Opcode.to_enum fr.opcode in
    let payload_len =
      match len with
      | n when n < 126 -> len
      | n when n < 1 lsl 16 -> 126
      | _ -> 127
    in
    let hdr = set_bit 0 15 fr.final in
    (* We do not support extensions for now *)
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 (is_client mode) in
    let hdr = hdr lor payload_len in
    (* Payload len is guaranteed to fit in 7 bits *)
    EndianBytes.BigEndian.set_int16 scratch 0 hdr;
    Buffer.add_subbytes buf scratch 0 2;
    (match len with
    | n when n < 126 -> ()
    | n when n < 1 lsl 16 ->
        EndianBytes.BigEndian.set_int16 scratch 0 n;
        Buffer.add_subbytes buf scratch 0 2
    | n ->
        EndianBytes.BigEndian.set_int64 scratch 0 Int64.(of_int n);
        Buffer.add_subbytes buf scratch 0 8);
    (match mode with
    | Server -> ()
    | Client random_string ->
        let mask = random_string 4 in
        Buffer.add_string buf mask;
        if len > 0 then xor mask content);
    Buffer.add_bytes buf content

  let close_with_code mode buf oc code =
    Buffer.clear buf;
    write_frame_to_buf ~mode buf @@ Frame.close code;
    write oc @@ Buffer.contents buf

  let read_frame ?max_len ic oc buf mode hdr =
    let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
    let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
    let final = is_bit_set 7 hdr_part1 in
    let extension = int_value 4 3 hdr_part1 in
    let opcode = int_value 0 4 hdr_part1 in
    let frame_masked = is_bit_set 7 hdr_part2 in
    let length = int_value 0 7 hdr_part2 in
    let opcode = Frame.Opcode.of_enum opcode in
    Buffer.clear buf;
    (match length with
    | i when i < 126 -> return i
    | 126 -> (
        read_uint16 ic buf >>= function
        | Some i -> return i
        | None -> return @@ -1)
    | 127 -> (
        read_int64 ic buf >>= function
        | Some i -> return i
        | None -> return @@ -1)
    | _ -> return @@ -1)
    >>= fun payload_len ->
    if payload_len = -1 then proto_error "payload len = %d" length
    else if extension <> 0 then
      close_with_code mode buf oc 1002 >>= fun () ->
      proto_error "unsupported extension"
    else if (match max_len with Some max -> payload_len > max | None -> false)
    then
      close_with_code mode buf oc 1009 >>= fun () ->
      proto_error "frame payload too big"
    else if Frame.Opcode.is_ctrl opcode && payload_len > 125 then
      close_with_code mode buf oc 1002 >>= fun () ->
      proto_error "control frame too big"
    else
      (if frame_masked then (
       Buffer.clear buf;
       read_exactly ic 4 buf >>= function
       | None -> proto_error "could not read mask"
       | Some mask -> return mask)
      else return String.empty)
      >>= fun mask ->
      if payload_len = 0 then
        return @@ Frame.create ~opcode ~extension ~final ()
      else (
        Buffer.clear buf;
        read_exactly ic payload_len buf >>= fun payload ->
        match payload with
        | None -> proto_error "could not read payload (len=%d)" payload_len
        | Some payload ->
            let payload = Bytes.of_string payload in
            if frame_masked then xor mask payload;
            let frame = Frame.of_bytes ~opcode ~extension ~final payload in
            return frame)

  let make_read_frame ?max_len ?(buf = Buffer.create 128) ~mode ic oc () =
    Buffer.clear buf;
    read_exactly ic 2 buf >>= function
    | None -> raise End_of_file
    | Some hdr -> read_frame ?max_len ic oc buf mode hdr

  module Request = Cohttp.Request.Make (IO)
  module Response = Cohttp.Response.Make (IO)

  module Connected_client = struct
    type t = {
      buffer : Buffer.t;
      endp : Conduit.endp;
      ic : Request.IO.ic;
      oc : Request.IO.oc;
      http_request : Cohttp.Request.t;
      standard_frame_replies : bool;
      read_frame : unit -> Frame.t IO.t;
    }

    let source { endp; _ } = endp

    let create ?max_len ?read_buf ?(write_buf = Buffer.create 128) http_request endp ic
        oc =
      let read_frame = make_read_frame ?max_len ?buf:read_buf ~mode:Server ic oc in
      {
        buffer = write_buf;
        endp;
        ic;
        oc;
        http_request;
        standard_frame_replies = false;
        read_frame;
      }

    let send { buffer; oc; _ } frame =
      Buffer.clear buffer;
      write_frame_to_buf ~mode:Server buffer frame;
      IO.write oc @@ Buffer.contents buffer

    let send_multiple { buffer; oc; _ } frames =
      Buffer.clear buffer;
      List.iter (write_frame_to_buf ~mode:Server buffer) frames;
      IO.write oc @@ Buffer.contents buffer

    let standard_recv t =
      t.read_frame () >>= fun fr ->
      match fr.Frame.opcode with
      | Frame.Opcode.Ping ->
          send t @@ Frame.create ~opcode:Frame.Opcode.Pong () >>= fun () ->
          return fr
      | Frame.Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          (if String.length fr.Frame.content >= 2 then
           send t
           @@ Frame.create ~opcode:Frame.Opcode.Close
                ~content:
                  String.(
                    sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)
                ()
          else send t @@ Frame.close 1000)
          >>= fun () -> return fr
      | _ -> return fr

    let recv t =
      if t.standard_frame_replies then standard_recv t else t.read_frame ()

    let http_request { http_request; _ } = http_request
    let make_standard t = { t with standard_frame_replies = true }
  end
end
