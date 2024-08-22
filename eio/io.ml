open Websocket
open Astring
open Eio

type mode = Client of (int -> string) | Server

let is_client mode = mode <> Server

let xor mask msg =
  for i = 0 to Bytes.length msg - 1 do
    (* masking msg to send *)
    Bytes.set msg i
      Char.(to_int mask.[i mod 4] lxor to_int (Bytes.get msg i) |> of_byte)
  done

let is_bit_set idx v = (v lsr idx) land 1 = 1
let set_bit v idx b = if b then v lor (1 lsl idx) else v land lnot (1 lsl idx)
let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

let read_exactly src remaining =
  try
    Some (Buf_read.take remaining src)
  with End_of_file -> None

let read_uint16 ic =
  match read_exactly ic 2 with
  | None -> None
  | Some s -> Some (EndianString.BigEndian.get_uint16 s 0)

let read_int64 ic =
  match read_exactly ic 8 with
  | None -> None
  | Some s -> Some (Int64.to_int @@ EndianString.BigEndian.get_int64 s 0)

let write_frame_to_buf ~mode buf fr =
  let open Frame in
  let content = Bytes.unsafe_of_string fr.content in
  let len = Bytes.length content in
  let opcode = Opcode.to_enum fr.opcode in
  let payload_len =
    match len with
    | n when n < 126 -> len
    | n when n < 1 lsl 16 -> 126
    | _ -> 127 in
  let hdr = set_bit 0 15 fr.final in
  (* We do not support extensions for now *)
  let hdr = hdr lor (opcode lsl 8) in
  let hdr = set_bit hdr 7 (is_client mode) in
  let hdr = hdr lor payload_len in
  (* Payload len is guaranteed to fit in 7 bits *)
  Buf_write.BE.uint16 buf hdr;
  ( match len with
  | n when n < 126 -> ()
  | n when n < 1 lsl 16 ->
      Buf_write.BE.uint16 buf n
  | n ->
      Buf_write.BE.uint64 buf Int64.(of_int n);
  );
  ( match mode with
  | Server -> ()
  | Client random_string ->
      let mask = random_string 4 in
      Buf_write.string buf mask ;
      if len > 0 then xor mask content ) ;
  Buf_write.bytes buf content

let close_with_code mode dst code =
  write_frame_to_buf ~mode dst @@ Frame.close code

let read_frame ic oc mode hdr =
  let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
  let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
  let final = is_bit_set 7 hdr_part1 in
  let extension = int_value 4 3 hdr_part1 in
  let opcode = int_value 0 4 hdr_part1 in
  let frame_masked = is_bit_set 7 hdr_part2 in
  let length = int_value 0 7 hdr_part2 in
  let opcode = Frame.Opcode.of_enum opcode in
  let payload_len =
    match length with
    | i when i < 126 -> i
    | 126 -> ( match read_uint16 ic with Some i -> i | None -> -1 )
    | 127 -> ( match read_int64 ic with Some i -> i | None -> -1 )
    | _ -> -1 in
  if payload_len = -1 then proto_error "payload len = %d" length
  else if extension <> 0 then (
    close_with_code mode oc 1002 ;
    proto_error "unsupported extension" )
  else if Frame.Opcode.is_ctrl opcode && payload_len > 125 then (
    close_with_code mode oc 1002 ;
    proto_error "control frame too big" )
  else
    let mask =
      if frame_masked then (
        match read_exactly ic 4 with
        | None -> proto_error "could not read mask"
        | Some mask -> mask )
      else String.empty in
    if payload_len = 0 then Frame.create ~opcode ~extension ~final ()
    else (
      match read_exactly ic payload_len with
      | None -> proto_error "could not read payload (len=%d)" payload_len
      | Some payload ->
          let payload = Bytes.unsafe_of_string payload in
          if frame_masked then xor mask payload ;
          let frame = Frame.of_bytes ~opcode ~extension ~final payload in
          frame )

let make_read_frame ~mode ic oc () =
  match read_exactly ic 2 with
  | None -> raise End_of_file
  | Some hdr -> read_frame ic oc mode hdr

module Connected_client = struct
  type t =
    { buffer: Buf_write.t;
      endp: Conduit.endp;
      ic: Buf_read.t;
      http_request: Cohttp.Request.t;
      standard_frame_replies: bool;
      read_frame: unit -> Frame.t }

  let source {endp; _} = endp

  let create http_request endp ic oc =
    let read_frame = make_read_frame ~mode:Server ic oc in
    { buffer = oc;
      endp;
      ic;
      http_request;
      standard_frame_replies = false;
      read_frame }

  let send {buffer; _} frame =
    write_frame_to_buf ~mode:Server buffer frame

  let send_multiple {buffer; _} frames =
    List.iter (write_frame_to_buf ~mode:Server buffer) frames

  let standard_recv t =
    let fr = t.read_frame () in
    match fr.Frame.opcode with
    | Frame.Opcode.Ping ->
        send t @@ Frame.create ~opcode:Frame.Opcode.Pong () ;
        fr
    | Frame.Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        if String.length fr.Frame.content >= 2 then
          send t
          @@ Frame.create ~opcode:Frame.Opcode.Close
               ~content:
                 String.(sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)
               ()
        else send t @@ Frame.close 1000 ;
        fr
    | _ -> fr

  let recv t =
    if t.standard_frame_replies then standard_recv t else t.read_frame ()

  let http_request {http_request; _} = http_request
  let make_standard t = {t with standard_frame_replies= true}
end
