(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
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

let random_string ?(base64=false) size =
  Nocrypto.Rng.generate size |>
  (if base64 then Nocrypto.Base64.encode else fun s -> s) |>
  Cstruct.to_string

let b64_encoded_sha1sum s =
  let open Nocrypto in
  Cstruct.of_string s |>
  Hash.SHA1.digest |>
  Base64.encode |>
  Cstruct.to_string

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
      | i when (i < 0 || i > 0xf) -> None
      | 0                         -> Some Continuation
      | 1                         -> Some Text
      | 2                         -> Some Binary
      | 8                         -> Some Close
      | 9                         -> Some Ping
      | 10                        -> Some Pong
      | i when i < 8              -> Some (Nonctrl i)
      | i                         -> Some (Ctrl i)

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

  type t = { opcode: Opcode.t;
             extension: int;
             final: bool;
             content: string;
           } [@@deriving show]

  let create
      ?(opcode=Opcode.Text)
      ?(extension=0)
      ?(final=true)
      ?(content="") () =
    {
      opcode; extension; final; content
    }

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
    Bytes.set msg i Char.(code mask.[i mod 4] lxor code (Bytes.get msg i) |> chr)
  done

let is_bit_set idx v =
  (v lsr idx) land 1 = 1

let set_bit v idx b =
  if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

let upgrade_present hs =
  Cohttp.Header.get_multi hs "connection" |> fun hs ->
  List.map (CCString.Split.list_cpy ~by:",") hs |> fun hs ->
  List.flatten hs |> fun hs ->
  List.map String.(fun h -> h |> lowercase |> trim) hs |>
  List.mem "upgrade"

module IO(IO: Cohttp.S.IO) = struct
  open IO

  let read_uint16 ic =
    read ic 2 >>= fun buf ->
    if String.length buf = 2 then
      return @@ EndianString.BigEndian.get_uint16 buf 0
    else failwith "read_uint16"

  let read_int64 ic =
    read ic 8 >>= fun buf ->
    if String.length buf = 2 then
      return @@ Int64.to_int @@ EndianString.BigEndian.get_int64 buf 0
    else failwith "read_int64"

  let write_frame_to_buf ~masked buf fr =
    let scratch = Bytes.create 8 in
    let open Frame in
    let mask = random_string 4 in
    let content = Bytes.unsafe_of_string fr.content in
    let len = Bytes.length content in
    let opcode = Opcode.to_enum fr.opcode in
    let payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let hdr = set_bit 0 15 (fr.final) in (* We do not support extensions for now *)
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 masked in
    let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
    EndianBytes.BigEndian.set_int16 scratch 0 hdr;
    Buffer.add_subbytes buf scratch 0 2;
    (match len with
     | n when n < 126 -> ()
     | n when n < (1 lsl 16) ->
       EndianBytes.BigEndian.set_int16 scratch 0 n;
       Buffer.add_subbytes buf scratch 0 2
     | n ->
       EndianBytes.BigEndian.set_int64 scratch 0 Int64.(of_int n);
       Buffer.add_subbytes buf scratch 0 8;
    );
    if masked && len > 0 then
      (xor mask content;
       Buffer.add_string buf mask
      );
    Buffer.add_bytes buf content

  (* ATTENTION: raise is used here, might fuck up Lwt! Always catch
         the exception if using Lwt. *)
  let make_read_frame ~masked (ic,oc) =
    let open Frame in
    let close_with_code code =
      let buf = Buffer.create 32 in
      write_frame_to_buf ~masked buf @@ Frame.close code;
      write oc @@ Buffer.contents buf >>= fun () ->
      raise Exit in
    fun () ->
      (read ic 2 >>= fun hdr ->
       if String.length hdr = 2 then return hdr
       else failwith "read header"
      ) >>= fun hdr ->
      if String.length hdr < 2 then raise Exit;
      let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
      let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
      let final = is_bit_set 7 hdr_part1 in
      let extension = int_value 4 3 hdr_part1 in
      let opcode = int_value 0 4 hdr_part1 in
      let frame_masked = is_bit_set 7 hdr_part2 in
      let length = int_value 0 7 hdr_part2 in
      let opcode = Frame.Opcode.of_enum opcode |> CCOpt.get_exn in
      (match length with
       | i when i < 126 -> return i
       | 126            -> read_uint16 ic
       | 127            -> read_int64 ic
       | _              -> assert false) >>= fun payload_len ->
      (if extension <> 0 then close_with_code 1002 else return ()) >>= fun () ->
      (if Opcode.is_ctrl opcode && payload_len > 125 then close_with_code 1002
       else return ()) >>= fun () ->
      (if frame_masked
       then read ic 4 >>= fun mask ->
         if String.length mask = 4 then return mask
         else failwith "read mask"
       else return "") >>= fun mask ->
      (* Create a buffer that will be passed to the push function *)
      (read ic payload_len >>= fun payload ->
       if String.length payload = payload_len then return payload
       else failwith "read payload")
      >>= fun payload ->
      let payload = Bytes.unsafe_of_string payload in
      let () = if frame_masked then xor mask payload in
      let frame = Frame.of_bytes ~opcode ~extension ~final payload in
      return frame
end
