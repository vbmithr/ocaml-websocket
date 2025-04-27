open Lwt.Infix
open Websocket_lwt_unix

let src = Logs.Src.create "wscat"

module Lo = (val Logs.src_log src : Logs.LOG)

let client uri =
  let open Websocket in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
  connect ~ctx client uri >>= fun conn ->
  let close_sent = ref false in
  let rec react () =
    Websocket_lwt_unix.read conn >>= function
    | { Frame.opcode = Ping; _ } ->
        write conn (Frame.create ~opcode:Pong ()) >>= react
    | { opcode = Close; content; _ } ->
        (* Immediately echo and pass this last message to the user *)
        (if !close_sent then Lwt.return_unit
         else if String.length content >= 2 then
           write conn
             (Frame.create ~opcode:Close ~content:(String.sub content 0 2) ())
         else write conn (Frame.close 1000))
        >>= fun () -> Websocket_lwt_unix.close_transport conn
    | { opcode = Pong; _ } -> react ()
    | { opcode = Text; content; _ } | { opcode = Binary; content; _ } ->
        Lwt_io.printf "> %s\n> %!" content >>= react
    | _ -> Websocket_lwt_unix.close_transport conn
  in
  let rec pushf () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
        Lo.debug (fun m -> m "Got EOF. Sending a close frame");
        write conn (Frame.create ~opcode:Close ()) >>= fun () ->
        close_sent := true;
        pushf ()
    | Some content -> write conn (Frame.create ~content ()) >>= pushf
  in
  pushf () <?> react ()

let rec react client client_id =
  let open Websocket in
  Connected_client.recv client >>= fun fr ->
  Lo.debug (fun m -> m "Client %d: %S" client_id Frame.(show fr));
  match fr.opcode with
  | Frame.Opcode.Ping ->
      Connected_client.send client
        Frame.(create ~opcode:Opcode.Pong ~content:fr.content ())
      >>= fun () -> react client client_id
  | Close ->
      (* Immediately echo and pass this last message to the user *)
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        Connected_client.send client
          Frame.(create ~opcode:Opcode.Close ~content ())
      else Connected_client.send client @@ Frame.close 1000
  | Pong -> react client client_id
  | Text | Binary ->
      Connected_client.send client fr >>= fun () -> react client client_id
  | _ -> Connected_client.send client Frame.(close 1002)

let server uri =
  let id = ref (-1) in
  let echo_fun client =
    incr id;
    let id = !id in
    Lo.info (fun m -> m "Connection from client id %d" id);
    Lwt.catch
      (fun () -> react client id)
      (fun exn ->
        Lo.err (fun m -> m "Client %d error" id);
        Lwt.fail exn)
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  let endp_str = endp |> Conduit.sexp_of_endp |> Sexplib.Sexp.to_string_hum in
  Lo.info (fun m -> m "endp = %s" endp_str);
  let ctx = Lazy.force default_ctx in
  endp_to_server ~ctx endp >>= fun server ->
  let server_str = server |> sexp_of_server |> Sexplib.Sexp.to_string_hum in
  Lo.info (fun m -> m "server = %s" server_str);
  establish_server ~ctx ~mode:server echo_fun

let main is_server uri =
  if !is_server then (
    ignore @@ server uri;
    fst @@ Lwt.wait ())
  else client uri

let apply_loglevel = function
  | 2 -> Logs.set_level ~all:true (Some Info)
  | 3 -> Logs.set_level ~all:true (Some Debug)
  | _ -> ()

let () =
  let uri = ref "" in
  let server = ref false in
  let speclist =
    Arg.align
      [
        ("-s", Arg.Set server, " Run as server");
        ("-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel");
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run (main server (Uri.of_string !uri))
