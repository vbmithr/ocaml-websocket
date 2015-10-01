open Lwt.Infix
open Websocket_lwt

let section = Lwt_log.Section.make "wscat"

let client uri =
  let open Frame in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp >>= fun client ->
  with_connection ~ctx:default_ctx client uri) >>= fun (recv, send) ->
  let react fr =
    match fr.opcode with
    | Opcode.Ping -> send @@ Frame.create ~opcode:Opcode.Pong ()

    | Opcode.Close ->
      (* Immediately echo and pass this last message to the user *)
      (if String.length fr.content >= 2 then
        send @@ Frame.create ~opcode:Opcode.Close
          ~content:(String.sub fr.content 0 2) ()
       else send @@ Frame.close 1000) >>= fun () ->
      Lwt.fail Exit

    | Opcode.Pong -> Lwt.return_unit

    | Opcode.Text
    | Opcode.Binary -> Lwt_io.printf "> %s\n> %!" fr.content

    | _ ->
      send @@ Frame.close 1002 >>= fun () ->
      Lwt.fail Exit
  in
  let rec react_forever () = recv () >>= react >>= react_forever
  in
  let rec pushf () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
      Lwt_log.debug ~section "Got EOF. Sending a close frame." >>= fun () ->
      send @@ Frame.close 1000 >>= pushf
    | Some content ->
      send @@ Frame.create ~content () >>= pushf
  in pushf () <?> react_forever ()

let server uri =
  let echo_fun id req recv send =
    let open Frame in
    let rec react () =
      recv () >>= fun fr ->
      Lwt_log.debug_f ~section "Client %d: %s" id Frame.(show fr) >>= fun () ->
      match fr.opcode with
      | Opcode.Ping ->
          send @@
          Frame.create ~opcode:Opcode.Pong ~content:fr.content () >>= fun () ->
          (* Immediately echo and pass this last message to the user *)
          if String.length fr.content >= 2 then
            send @@ Frame.create ~opcode:Opcode.Close
              ~content:(String.sub fr.content 0 2) () >>= react
          else
            send @@ Frame.close 1000
      | Opcode.Pong -> react ()

      | Opcode.Text
      | Opcode.Binary ->
          send fr >>= react
      | _ ->
          send @@ Frame.close 1002
    in
    Lwt_log.info_f ~section "Connection from client id %d" id >>= fun () ->
    try%lwt react () with exn ->
      Lwt_log.error_f ~section ~exn "Client %d error" id >>= fun () ->
      Lwt.fail exn
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(
    endp_to_server ~ctx:default_ctx endp >>= fun server ->
    establish_server ~ctx:default_ctx ~mode:server echo_fun
  )

let main is_server uri =
  if !is_server then (ignore @@ server uri; fst @@ Lwt.wait ())
  else client uri

let _ =
  let uri = ref "" in
  let server = ref false in

  let speclist = Arg.align
      [
        "-s", Arg.Set server, " Run as server";
        "-v", Arg.String (fun s -> Lwt_log.(add_rule s Info)), "<section> Put <section> to Info level";
        "-vv", Arg.String (fun s -> Lwt_log.(add_rule s Debug)), "<section> Put <section> to Debug level"
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run @@ main server @@ Uri.of_string !uri
