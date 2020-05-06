open Lwt.Infix
open Websocket
open Websocket_lwt_unix

let section = Lwt_log.Section.make "wscat"

let resolvers =
  Conduit_lwt.register_resolver ~key:Conduit_lwt_unix_tcp.endpoint
    (Conduit_lwt_unix_tcp.resolv_conf ~port:80)
    Conduit.empty

let client uri =
  let open Frame in
  with_connection ~resolvers uri >>= fun (recv, send) ->
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

let react client client_id =
  let rec inner () =
    Connected_client.recv client >>= fun fr ->
    Lwt_log.debug_f ~section "Client %d: %s" client_id Frame.(show fr) >>= fun () ->
    match fr.opcode with
    | Frame.Opcode.Ping ->
      Connected_client.send client
        Frame.(create ~opcode:Opcode.Pong ~content:fr.content ()) >>=
      inner
    | Frame.Opcode.Close ->
      (* Immediately echo and pass this last message to the user *)
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        Connected_client.send client Frame.(create ~opcode:Opcode.Close ~content ())
      else
      Connected_client.send client @@ Frame.close 1000
    | Frame.Opcode.Pong ->
      inner ()
    | Frame.Opcode.Text
    | Frame.Opcode.Binary ->
      Connected_client.send client fr >>=
      inner
    | _ ->
      Connected_client.send client Frame.(close 1002)
  in inner ()

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let option_fold ~none = function
  | Some x -> x
  | None -> none

let server uri =
  let id = ref (-1) in
  let echo_fun client =
    incr id;
    let id = !id in
    Lwt_log.info_f ~section "Connection from client id %d" id >>= fun () ->
    Lwt.catch
      (fun () -> react client id)
      (fun exn ->
         Lwt_log.error_f ~section ~exn "Client %d error" id >>= fun () ->
         Lwt.fail exn)
  in
  let sockaddr = match option_map Unix.gethostbyname (Uri.host uri), Uri.port uri with
    | Some { Unix.h_addr_list; _ }, port when Array.length h_addr_list > 0 ->
      Unix.ADDR_INET (h_addr_list.(0), option_fold ~none:80 port)
    | Some _, port | None, port ->
      Unix.ADDR_INET (Unix.inet_addr_loopback, option_fold ~none:80 port)
    | exception _ ->
      let port = Uri.port uri in
      Unix.ADDR_INET (Unix.inet_addr_loopback, option_fold ~none:80 port) in
  let key = Conduit_lwt_unix_tcp.configuration in
  let service = Conduit_lwt_unix_tcp.service in
  let cfg =
    { Conduit_lwt_unix_tcp.sockaddr
    ; capacity= 40 } in
  establish_server ~key service cfg echo_fun

let main is_server uri =
  if !is_server then (ignore @@ server uri; fst @@ Lwt.wait ())
  else client uri

let apply_loglevel = function
| 2 -> Lwt_log.(add_rule "*" Info)
| 3 -> Lwt_log.(add_rule "*" Debug)
| _ -> ()

let () =
  let uri = ref "" in
  let server = ref false in
  let speclist = Arg.align [
      "-s", Arg.Set server, " Run as server";
      "-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel";
    ] in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run (main server (Uri.of_string !uri))
