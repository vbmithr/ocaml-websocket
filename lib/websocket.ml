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

open Astring

let b64_encoded_sha1sum s = Sha1.sha_1 s |> B64.encode ~pad:true

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

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
      | Nonctrl of int [@@deriving show]

    let min = 0x0
    let max = 0xf

    let of_enum = function
      | i when (i < 0 || i > 0xf) -> invalid_arg "Frame.Opcode.of_enum"
      | 0                         -> Continuation
      | 1                         -> Text
      | 2                         -> Binary
      | 8                         -> Close
      | 9                         -> Ping
      | 10                        -> Pong
      | i when i < 8              -> Nonctrl i
      | i                         -> Ctrl i

    let to_enum = function
      | Continuation   -> 0
      | Text           -> 1
      | Binary         -> 2
      | Close          -> 8
      | Ping           -> 9
      | Pong           -> 10
      | Ctrl i         -> i
      | Nonctrl i      -> i

    let is_ctrl opcode = to_enum opcode > 7
  end

  type t = {
    opcode: Opcode.t [@default Opcode.Text];
    extension: int [@default 0];
    final: bool [@default true];
    content: string [@default ""];
  } [@@deriving create,show]

  let of_bytes ?opcode ?extension ?final content =
    let content = Bytes.unsafe_to_string content in
    create ?opcode ?extension ?final ~content ()

  let close code =
    let content = Bytes.create 2 in
    EndianBytes.BigEndian.set_int16 content 0 code;
    of_bytes ~opcode:Opcode.Close content

  let of_subbytes ?opcode ?extension ?final content pos len =
    let content = Bytes.(sub content pos len |> unsafe_to_string) in
    create ?opcode ?extension ?final ~content ()
end

let xor mask msg =
  for i = 0 to Bytes.length msg - 1 do (* masking msg to send *)
    Bytes.set msg i Char.(to_int mask.[i mod 4] lxor to_int (Bytes.get msg i) |> of_byte)
  done

let is_bit_set idx v =
  (v lsr idx) land 1 = 1

let set_bit v idx b =
  if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

let upgrade_present hs =
  Cohttp.Header.get_multi hs "connection" |> fun hs ->
  List.map (String.cuts ~sep:",") hs |> fun hs ->
  List.flatten hs |> fun hs ->
  List.map String.(fun h -> h |> String.Ascii.lowercase |> trim) hs |>
  List.mem "upgrade"

module IO(IO: Cohttp.S.IO) = struct
  open IO

  let rec read_exactly ic remaining buf =
    read ic remaining >>= fun s ->
    if s = "" then return None
    else
      let recv_len = String.length s in
      Buffer.add_string buf s;
      if remaining - recv_len <= 0 then return @@ Some (Buffer.contents buf)
      else read_exactly ic (remaining - recv_len) buf

  let read_uint16 ic buf =
    read_exactly ic 2 buf >>= fun s ->
    match s with
    | None -> failwith "read_uint16"
    | Some s ->
        return @@ EndianString.BigEndian.get_uint16 s 0

  let read_int64 ic buf =
    read_exactly ic 8 buf >>= fun s ->
    match s with
    | None -> failwith "read_int64"
    | Some s ->
        return @@ Int64.to_int @@ EndianString.BigEndian.get_int64 s 0

  let write_frame_to_buf ?(random_string=Rng.std ?state:None) ~masked buf fr =
    let scratch = Bytes.create 8 in
    let open Frame in
    let mask = random_string 4 in
    let content = Bytes.unsafe_of_string fr.content in
    let len = Bytes.length content in
    let opcode = Opcode.to_enum fr.opcode in
    let payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127
    in
    let hdr = set_bit 0 15 (fr.final) in (* We do not support extensions for now *)
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 masked in
    let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
    EndianBytes.BigEndian.set_int16 scratch 0 hdr;
    Buffer.add_subbytes buf scratch 0 2;
    begin match len with
     | n when n < 126 -> ()
     | n when n < (1 lsl 16) ->
       EndianBytes.BigEndian.set_int16 scratch 0 n;
       Buffer.add_subbytes buf scratch 0 2
     | n ->
       EndianBytes.BigEndian.set_int64 scratch 0 Int64.(of_int n);
       Buffer.add_subbytes buf scratch 0 8;
    end;
    if masked then begin
      Buffer.add_string buf mask;
      if len > 0 then xor mask content;
    end;
    Buffer.add_bytes buf content

  let make_read_frame ?random_string ~masked (ic,oc) =
    let buf = Buffer.create 4096 in
    let open Frame in
    let close_with_code code =
      Buffer.clear buf;
      write_frame_to_buf ?random_string ~masked buf @@ Frame.close code;
      write oc @@ Buffer.contents buf
    in
    fun () ->
      Buffer.clear buf;
      read_exactly ic 2 buf >>= fun hdr ->
      match hdr with
      | None -> failwith "EOF"
      | Some hdr ->
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
       | 126 -> (try read_uint16 ic buf with _ -> return @@ -1)
       | 127 -> (try read_int64 ic buf with _ -> return @@ -1)
       | _ -> return @@ -1
      ) >>= fun payload_len ->
      if payload_len = -1 then
        failwith @@ "payload len = " ^ string_of_int length
      else if extension <> 0 then
        close_with_code 1002 >>= fun () ->
        failwith "unsupported extension"
      else if Opcode.is_ctrl opcode && payload_len > 125 then
        close_with_code 1002 >>= fun () ->
        failwith "control frame too big"
      else
        (if frame_masked then
           read_exactly ic 4 (Buffer.create 4) >>= function
           | None -> failwith "could not read mask";
           | Some mask -> return mask
         else return String.empty) >>= fun mask ->
        if payload_len = 0 then
          return @@ Frame.create ~opcode ~extension ~final ()
        else
          (Buffer.clear buf;
          read_exactly ic payload_len buf) >>= fun payload ->
          match payload with
          | None -> failwith "could not read payload"
          | Some payload ->
          let payload = Bytes.unsafe_of_string payload in
          if frame_masked then xor mask payload;
          let frame = Frame.of_bytes ~opcode ~extension ~final payload in
          return frame
end
