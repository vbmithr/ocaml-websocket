(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
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

module CK = Cryptokit
module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

#if ocaml_version < (4,1)
let (@@) f x = f x
let (|>) x f = f x
#endif

let (>>=) = Lwt.bind
let (>|=) x f = Lwt.map f x
let (<&>) a b = Lwt.join [a; b]
let (<?>) a b = Lwt.pick [a; b]

let base64_encode str =
  let tr = CK.Base64.encode_compact_pad () in
  CK.transform_string tr str

let sha1sum str =
  let hash = CK.Hash.sha1 () in
  CK.hash_string hash str

module Opt = struct
  let bind x f = match x with None -> None | Some v -> f v
  let (>>=) = bind

  let try_bind x f e = match bind x f with None -> e | r -> r

  let map x f = match x with None -> None | Some v -> Some (f v)
  let (>|=) = map

  let run_exc = function
    | None -> raise (Invalid_argument "run_exc")
    | Some v -> v

  let run d = function
    | None -> d
    | Some v -> v
end

exception Not_implemented

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

module Frame = struct
  type opcode =
    [ `Continuation
    | `Text
    | `Binary
    | `Close
    | `Ping
    | `Pong
    | `Ctrl of int
    | `Nonctrl of int
    ]

  type t = { opcode    : opcode;
             extension : int;
             final     : bool;
             content   : string }

  let opcode f    = f.opcode
  let extension f = f.extension
  let final f     = f.final
  let content f   = f.content

  let of_string ?(opcode=`Text) ?(extension=0) ?(final=true) content =
    { opcode; extension; final; content }
end

let string_of_opcode = function
  | `Continuation -> "continuation frame"
  | `Text         -> "text frame"
  | `Binary       -> "binary frame"
  | `Close        -> "close frame"
  | `Ping         -> "ping frame"
  | `Pong         -> "pong frame"
  | `Ctrl i       -> Printf.sprintf "control frame code %d" i
  | `Nonctrl i    -> Printf.sprintf "non-control frame code %d" i

let opcode_of_int i = match i land 0xf with
  | 0                     -> `Continuation
  | 1                     -> `Text
  | 2                     -> `Binary
  | 8                     -> `Close
  | 9                     -> `Ping
  | 10                    -> `Pong
  | i when i > 2 && i < 8 -> `Nonctrl i
  | i                     -> `Ctrl i

let int_of_opcode = function
  | `Continuation -> 0
  | `Text         -> 1
  | `Binary       -> 2
  | `Close        -> 8
  | `Ping         -> 9
  | `Pong         -> 10
  | `Ctrl i       -> i
  | `Nonctrl i    -> i

let xor mask msg =
  for i = 0 to String.length msg - 1 do (* masking msg to send *)
    msg.[i] <- Char.chr @@
      Char.code mask.[i mod 4] lxor Char.code msg.[i]
  done

let read_int16 ic =
  let buf = String.create 2 in
  Lwt_io.read_into_exactly ic buf 0 2 >|= fun () ->
  EndianString.BigEndian.get_int16 buf 0

let read_int64 ic =
  let buf = String.create 8 in
  Lwt_io.read_into_exactly ic buf 0 8 >|= fun () ->
  EndianString.BigEndian.get_int64 buf 0

let write_int16 oc v =
  let buf = String.create 2 in
  EndianString.BigEndian.set_int16 buf 0 v;
  Lwt_io.write oc buf

let write_int64 oc v =
  let buf = String.create 8 in
  EndianString.BigEndian.set_int64 buf 0 v;
  Lwt_io.write oc buf

let is_bit_set idx v =
  (v lsr idx) land 1 = 1

let set_bit v idx b =
  if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

let int_value shift len v =
  (v lsr shift) land ((1 lsl len) - 1)

(* Should an exception arise in the body of this function, ic and oc
   will be closed *)
let read_frames (ic,oc) push =
  let hdr = String.create 2 in
  let mask = String.create 4 in
  let rec inner () =
    lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
    let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
    let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
    let final = is_bit_set 7 hdr_part1 in
    let extension = int_value 4 3 hdr_part1 in
    let opcode = int_value 0 4 hdr_part1 in
    let masked = is_bit_set 7 hdr_part2 in
    let length = int_value 0 7 hdr_part2 in
    let opcode = opcode_of_int opcode in
    lwt payload_len = (match length with
        | i when i < 126 -> Lwt.return @@ Int64.of_int i
        | 126            -> read_int16 ic >|= Int64.of_int
        | 127            -> read_int64 ic
        | _              -> raise_lwt (Failure "internal error"))
      >|= Int64.to_int
    in
    let payload_len = if `Close = opcode then payload_len - 2 else payload_len in
    lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 4 else Lwt.return_unit in
    lwt _ = if `Close = opcode then Lwt_io.read ic ~count:2 else Lwt.return "" in
    (* Create a buffer that will be passed to the push function *)
    let content = String.create payload_len in
    lwt () = Lwt_io.read_into_exactly ic content 0 payload_len in
    let () = if masked then xor mask content in
    push (Some (Frame.of_string ~opcode ~extension ~final content));
    Lwt.return_unit
  in
  let rec inner_cleanup () =
    (try_lwt inner ()
    with exn ->
      push None;
      Lwt_io.(close ic <&> close oc) >> raise_lwt exn) >>
    inner_cleanup () in
  inner_cleanup ()

(* Good enough, and do not eat entropy *)
let myrng = CK.Random.device_rng "/dev/urandom"

(* Should an exception arise in the body of this function, ic and oc
   will be closed *)
let write_frames ~masked stream (ic,oc) =
  let send_frame fr =
    let mask = CK.Random.string myrng 4 in
    let len = String.length (Frame.content fr) in
    let opcode = int_of_opcode (Frame.opcode fr) in
    let isclose = `Close = Frame.opcode fr in
    let payload_len = if isclose then len + 2 else len in
    let payload_len = match payload_len with
      | n when n < 126      -> payload_len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let hdr = set_bit 0 15 (Frame.final fr) in (* We do not support extensions for now *)
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 masked in
    let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
    let hdr_string = String.create 2 in
    let () = EndianString.BigEndian.set_int16 hdr_string 0 hdr in
    lwt () = Lwt_io.write_from_exactly oc hdr_string 0 2 in
    lwt () =
      (match len with
        | n when n < 126        -> Lwt.return ()
        | n when n < (1 lsl 16) -> write_int16 oc n
        | n                     -> Int64.of_int n |> write_int64 oc)
    in
    lwt () = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask (Frame.content fr) else Lwt.return () in
    lwt () = if isclose then write_int16 oc 1000 else Lwt.return_unit in
    lwt () = Lwt_io.write_from_exactly oc (Frame.content fr) 0 len in
    Lwt_io.flush oc in
  let rec inner_cleanup () =
    (try_lwt
       Lwt_stream.next stream >>= send_frame
     with exn ->
       Lwt_io.(close ic <&> close oc) >> raise_lwt exn) >>
    inner_cleanup () in
  inner_cleanup ()

let setup_socket = Lwt_io_ext.set_tcp_nodelay

exception No_response_from_remote_server
exception HTTP_Error of string

let is_upgrade = 
    let open Re in
    let re = compile (seq [ rep any; no_case (str "upgrade") ]) in
    (function None -> false
            | Some(key) -> execp re key)

let open_connection ?(tls = false) ?(extra_headers = []) uri =
  (* Initialisation *)
  lwt myhostname = Lwt_unix.gethostname () in
  let host = Opt.run_exc (Uri.host uri) in
  let port = Uri.port uri in
  let scheme = Uri.scheme uri in
  let port, tls = match port, scheme with
    | None, None -> 80, tls
    | Some p, _ -> p, tls
    | None, Some s -> (match s with "https" | "wss" -> 443, true | _ -> 80, tls) in

  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let connect () =
    let open Cohttp in
    let nonce = base64_encode @@ CK.Random.string myrng 16 in
    let in_extra_hdrs key = 
      let lkey = String.lowercase key in
      (List.find_all (fun (k,v) -> (String.lowercase k) = lkey) extra_headers) = [] in
    let hdr_list = extra_headers @ List.filter (fun (k,v) -> in_extra_hdrs k)
      ["Upgrade"               , "websocket";
       "Connection"            , "Upgrade";
       "Sec-WebSocket-Key"     , nonce;
       "Sec-WebSocket-Version" , "13"] in
    let headers = Header.of_list hdr_list in
    let req = Request.make ~headers uri in
    Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sockaddr ->
    Lwt_unix.handle_unix_error
      (fun () -> Lwt_io_ext.open_connection ~tls ~setup_socket sockaddr)
      () >>= fun (ic, oc) ->
    try_lwt
      Lwt_unix.handle_unix_error
        (fun () -> CU.Request.write (fun _ _ -> Lwt.return ()) req oc) () >>= fun () ->
      Lwt_unix.handle_unix_error
        (fun () -> CU.Response.read ic) () >>= (function
        | `Ok r -> Lwt.return r
        | `Eof | `Invalid _ -> raise_lwt No_response_from_remote_server)
      >>= fun response ->
      let status = Response.status response in
      let headers = CU.Response.headers response in
      if Code.(is_error (code_of_status status))
      then raise (HTTP_Error Code.(string_of_status status))
      else
        (* let _ = C.Header.fold (fun k v () -> Printf.printf "%s: %s\n%!" k v) headers () in *)
        (assert_lwt Response.version response = `HTTP_1_1) >>
        (assert_lwt status = `Switching_protocols) >>
        (assert_lwt Opt.(Header.get headers "upgrade" >|= String.lowercase) = Some "websocket") >>
        (assert_lwt (is_upgrade (C.Header.get headers "connection"))) >>
        (assert_lwt Header.get headers "sec-websocket-accept" =
         Some (nonce ^ websocket_uuid |> sha1sum |> base64_encode)) >>
        Lwt_log.notice_f "Connected to %s\n%!" (Uri.to_string uri) >>
        Lwt.return (ic, oc)
    with exn ->
      Lwt_io.(close ic <&> close oc) >> raise_lwt exn

  in
  lwt ic, oc = connect () in
  (* ic and oc will be closed by either read_frames or write_frames on
     failure *)
  Lwt.async (fun () ->
      (read_frames (ic,oc) push_in <&> write_frames ~masked:true stream_out (ic,oc)));
  Lwt.return (stream_in, push_out)

let with_connection ?(tls = false) ?(extra_headers = []) uri f =
  lwt stream_in, push_out = open_connection ~tls ~extra_headers uri in
  f (stream_in, push_out)

let establish_server ?(tls = false) ?buffer_size ?backlog sockaddr f =
  let server_fun (ic, oc) =
    let stream_in, push_in   = Lwt_stream.create ()
    and stream_out, push_out = Lwt_stream.create () in
    lwt request = CU.Request.read ic >>= function
      | `Ok r -> Lwt.return r
      | `Eof -> Lwt.fail Not_found
      | `Invalid reason -> Lwt.fail (Failure reason) in
    let meth    = C.Request.meth request
    and version = C.Request.version request
    and uri     = C.Request.uri request
    and headers = C.Request.headers request in
    let key = Opt.run_exc @@ C.Header.get headers "sec-websocket-key" in
    lwt () =
      (assert_lwt version = `HTTP_1_1) >>
      (assert_lwt meth = `GET) >>
      (assert_lwt Opt.(C.Header.get headers "upgrade" >|= String.lowercase) = Some "websocket") >>
      (assert_lwt (is_upgrade (C.Header.get headers "connection")))
    in
    let hash = key ^ websocket_uuid |> sha1sum |> base64_encode in
    let response_headers = C.Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = C.Response.make
        ~status:`Switching_protocols
        ~encoding:C.Transfer.Unknown
        ~headers:response_headers () in
    CU.Response.write (fun _ _ -> Lwt.return_unit) response oc >>
    Lwt.pick [read_frames (ic,oc) push_in;
              write_frames ~masked:false stream_out (ic,oc);
              f uri (stream_in, push_out)]
  in
  Lwt.async_exception_hook := (fun exn -> Printf.printf "EXN: %s\n%!" (Printexc.to_string exn));
  Lwt_io_ext.establish_server
    ~setup_server_socket:setup_socket
    ~setup_clients_sockets:setup_socket
    ?buffer_size ?backlog sockaddr
    (fun (ic,oc) -> Lwt.async (fun () ->
         let server_t = server_fun (ic,oc)
         in Lwt.on_termination server_t (fun () -> Lwt_io.(close ic <&> close oc) |> Lwt.ignore_result);
         server_t
       ))
