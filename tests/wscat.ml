open Lwt.Infix
open Websocket

let section = Lwt_log.Section.make "wscat"

let client uri =
  let open Frame in
  with_connection uri >>= fun (recv, send) ->
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

let server ?certificate sockaddr =
  let echo_fun id uri recv send =
    let open Frame in
    let react fr =
      match fr.opcode with
      | Opcode.Ping ->
        send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content ()

      | Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.content >= 2 then
           send @@ Frame.create ~opcode:Opcode.Close
             ~content:(String.sub fr.content 0 2) ()
         else send @@ Frame.close 1000) >>= fun () ->
        Lwt.fail Exit

      | Opcode.Pong -> Lwt.return_unit

      | Opcode.Text
      | Opcode.Binary -> send fr

      | _ ->
        send @@ Frame.close 1002 >>= fun () ->
        Lwt.fail Exit
    in
    let rec react_forever () = recv () >>= react >>= react_forever
    in
    react_forever ()
  in
  establish_server ?certificate sockaddr echo_fun

let _ =
  let server_port = ref "" in
  let endpoint_address = ref "" in
  let cert_dir = ref None in

  let speclist = Arg.align
      [
        "-tls", Arg.String (fun s -> cert_dir := Some s), "<cert_dir> Use TLS for the server";
        "-s", Arg.Set_string server_port, "<int> Run server on specified port";
        "-v", Arg.String (fun s -> Lwt_log.(add_rule s Info)), "<section> Put <section> to Info level";
        "-vv", Arg.String (fun s -> Lwt_log.(add_rule s Debug)), "<section> Put <section> to Debug level"
      ]
  in
  let anon_fun s = endpoint_address := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;

  let main () =
    Nocrypto_entropy_lwt.initialize () >>= fun () ->
    match !server_port, !endpoint_address with
    | p, "" when p <> "" ->
      begin
        Lwt_io_ext.sockaddr_of_dns "localhost" !server_port >>= fun sa ->
        let%lwt certificate = match !cert_dir with
          | None -> Lwt.return None
          | Some dir ->
            let cert = dir ^ "/server.crt" in
            let priv_key = dir ^ "/server.key" in
            X509_lwt.private_of_pems ~cert ~priv_key >|= fun c -> Some c
        in
        ignore (server ?certificate sa);
        fst @@ Lwt.wait ()
      end
    | _, endpoint when endpoint <> "" ->
      client (Uri.of_string !endpoint_address)
    | _ -> Lwt_io.eprintl "Please specify a server port or a valid URI."
  in
  Lwt_main.run (main ())
