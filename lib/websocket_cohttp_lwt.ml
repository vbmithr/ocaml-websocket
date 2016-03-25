module C = Cohttp
module Lwt_IO = Websocket.IO(Cohttp_lwt_unix_io)

open Lwt

let send_frames stream oc =
    let buf = Buffer.create 128 in
    let send_frame fr =
      Buffer.clear buf;
      Lwt_IO.write_frame_to_buf ~masked:false buf fr;
      Lwt_io.write oc @@ Buffer.contents buf
    in
    Lwt_stream.iter_s send_frame stream
  ;;

let read_frames icoc handler_fn =
    let read_frame () =
      let rf = Lwt_IO.make_read_frame ~masked:false icoc () in
      match%lwt rf with
      | `Ok frame -> Lwt.return frame
      | `Error msg -> Lwt.fail_with msg
    in
    while%lwt true do
      read_frame () >>= Lwt.wrap1 handler_fn
    done
  ;;

let upgrade_connection request conn incoming_handler =
  let headers = Cohttp.Request.headers request in
  let key = CCOpt.get_exn @@ Cohttp.Header.get headers "sec-websocket-key" in
  let hash = key ^ Websocket.websocket_uuid |> Websocket.b64_encoded_sha1sum in
  let response_headers =
      Cohttp.Header.of_list
        ["Upgrade", "websocket"
        ;"Connection", "Upgrade"
        ;"Sec-WebSocket-Accept", hash]
  in
  let resp =
      Cohttp.Response.make
        ~status:`Switching_protocols
        ~encoding:Cohttp.Transfer.Unknown
        ~headers:response_headers
        ~flush:true
        ()
  in

  let frames_out_stream, frames_out_fn = Lwt_stream.create () in

  let body_stream, stream_push = Lwt_stream.create () in
  let _ =
      let open Conduit_lwt_unix in
      match conn with
          | TCP (tcp : tcp_flow) ->
              let oc = Lwt_io.of_fd ~mode:Lwt_io.output tcp.fd in
              let ic = Lwt_io.of_fd ~mode:Lwt_io.input tcp.fd in
              Lwt.join [
                  (* input: data from the client is read from the input channel
                   * of the tcp connection; pass it to handler function *)
                  read_frames (ic, oc) incoming_handler;
                  (* output: data for the client is written to the output
                   * channel of the tcp connection *)
                  send_frames frames_out_stream oc;
              ]
          | _ -> Lwt.fail_with "expected TCP Websocket connection"
  in
  Lwt.return (resp, Cohttp_lwt_body.of_stream body_stream, frames_out_fn)
