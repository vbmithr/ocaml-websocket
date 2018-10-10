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

module type S = sig
  module IO : Cohttp.S.IO
  module Request : Cohttp.S.Http_io
    with type t = Cohttp.Request.t
     and type 'a IO.t = 'a IO.t
     and type IO.ic = IO.ic
     and type IO.oc = IO.oc
  module Response : Cohttp.S.Http_io
    with type t = Cohttp.Response.t
     and type 'a IO.t = 'a IO.t
     and type IO.ic = IO.ic
     and type IO.oc = IO.oc
  module Connected_client : sig
    type t

    val create :
      ?read_buf:Buffer.t -> ?write_buf:Buffer.t ->
      Cohttp.Request.t -> Conduit.endp -> IO.ic -> IO.oc -> t

    val make_standard : t -> t
    val send : t -> Websocket.Frame.t -> unit IO.t
    val send_multiple : t -> Websocket.Frame.t list -> unit IO.t
    val recv : t -> Websocket.Frame.t IO.t
    val http_request : t -> Cohttp.Request.t
    val source : t -> Conduit.endp
  end
end

module Make (IO : Cohttp.S.IO) = struct
  module IO = IO
  open IO
  module Lwt_IO = Websocket.IO(IO)
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  module Connected_client = struct
    type t = {
      buffer: Buffer.t;
      endp: Conduit.endp;
      ic: Request.IO.ic;
      oc: Request.IO.oc;
      http_request: Cohttp.Request.t;
      standard_frame_replies: bool;
      read_frame: unit -> Frame.t IO.t;
    }

    let source { endp ; _ } = endp

    let create
        ?read_buf
        ?(write_buf=Buffer.create 128)
        http_request endp ic oc =
      let read_frame = Lwt_IO.make_read_frame ?buf:read_buf ~mode:Server ic oc in
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
      Lwt_IO.write_frame_to_buf ~mode:Server buffer frame;
      IO.write oc @@ Buffer.contents buffer

    let send_multiple { buffer; oc; _ } frames =
      Buffer.clear buffer;
      List.iter (Lwt_IO.write_frame_to_buf ~mode:Server buffer) frames;
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
           send t @@ Frame.create
             ~opcode:Frame.Opcode.Close
             ~content:(String.(sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)) ()
         else send t @@ Frame.close 1000
        ) >>= fun () ->
        return fr
      | _ -> return fr

    let recv t =
      if t.standard_frame_replies then
        standard_recv t
      else
      t.read_frame ()

    let http_request { http_request; _ } = http_request
    let make_standard t = { t with standard_frame_replies = true }
  end
end
