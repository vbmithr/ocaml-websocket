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
  let id = ref (-1) in
  let echo_fun client =
    incr id;
    let id = !id in
    let open Frame in
    let rec react () =
      Connected_client.recv client >>= fun fr ->
      Lwt_log.debug_f ~section "Client %d: %s" id Frame.(show fr) >>= fun () ->
      match fr.opcode with
      | Opcode.Ping ->
        Connected_client.send client Frame.(create ~opcode:Opcode.Pong ~content:fr.content ()) >>=
        react
      | Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        if String.length fr.content >= 2 then
          let content = String.sub fr.content 0 2 in
          Connected_client.send client Frame.(create ~opcode:Opcode.Close ~content ())
        else
          Connected_client.send client @@ Frame.close 1000
      | Opcode.Pong ->
        react ()
      | Opcode.Text
      | Opcode.Binary ->
        Connected_client.send client fr >>=
        react
      | _ ->
        Connected_client.send client Frame.(close 1002)
    in
    Lwt_log.info_f ~section "Connection from client id %d" id >>= fun () ->
    try%lwt react () with exn ->
      Lwt_log.error_f ~section ~exn "Client %d error" id >>= fun () ->
      Lwt.fail exn
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  let endp_str = endp |> Conduit.sexp_of_endp |> Sexplib.Sexp.to_string_hum in
  Lwt_log.info_f ~section "endp = %s" endp_str >>= fun () ->
  endp_to_server ~ctx:default_ctx endp >>= fun server ->
  let server_str = server |> sexp_of_server |> Sexplib.Sexp.to_string_hum in
  Lwt_log.info_f ~section "server = %s" server_str >>= fun () ->
  establish_server ~ctx:default_ctx ~mode:server echo_fun

let main is_server uri =
  if !is_server then (ignore @@ server uri; fst @@ Lwt.wait ())
  else client uri

let apply_loglevel = function
  | 2 -> Lwt_log.(add_rule "*" Info)
  | 3 -> Lwt_log.(add_rule "*" Debug)
  | _ -> ()

let _ =
  let uri = ref "" in
  let server = ref false in

  let speclist = Arg.align
      [ "-s", Arg.Set server, " Run as server";
        "-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel";
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run @@ main server @@ Uri.of_string !uri
