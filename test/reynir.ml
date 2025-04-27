open Lwt.Infix
open Websocket
open Websocket_lwt_unix

let src = Logs.Src.create "reynir"

module Lo = (val Logs.src_log src : Logs.LOG)

let handler id client =
  incr id;
  let id = !id in
  let send = Connected_client.send client in
  Lo.info (fun m -> m "New connection (id = %d)" id);
  Lwt.async (fun () ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      send @@ Frame.create ~content:"Delayed message" ());
  let rec recv_forever () =
    let open Frame in
    let react fr =
      Lo.debug (fun m -> m "<- %s" (Frame.show fr));
      match fr.opcode with
      | Opcode.Ping ->
          send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content ()
      | Opcode.Close ->
          Lo.info (fun m -> m "Client %d sent a close frame" id);
          (* Immediately echo and pass this last message to the user *)
          (if String.length fr.content >= 2 then
             send
             @@ Frame.create ~opcode:Opcode.Close
                  ~content:(String.sub fr.content 0 2)
                  ()
           else send @@ Frame.close 1000)
          >>= fun () -> Lwt.fail Exit
      | Opcode.Pong -> Lwt.return_unit
      | Opcode.Text | Opcode.Binary -> send @@ Frame.create ~content:"OK" ()
      | _ -> send @@ Frame.close 1002 >>= fun () -> Lwt.fail Exit
    in
    Connected_client.recv client >>= react >>= recv_forever
  in
  Lwt.catch recv_forever (fun exn ->
      Lo.info (fun m -> m "Connection to client %d lost" id);
      Lwt.fail exn)

let main uri =
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  let ctx = Lazy.force default_ctx in
  endp_to_server ~ctx endp >>= fun server ->
  establish_server ~ctx ~mode:server (handler @@ ref (-1))

let () =
  let uri = ref "http://localhost:9001" in
  let speclist =
    Arg.align
      [
        ( "-v",
          Arg.String (fun _ -> Logs.set_level ~all:true (Some Info)),
          " Info level" );
        ( "-vv",
          Arg.String (fun _ -> Logs.set_level ~all:true (Some Info)),
          " Debug level" );
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  ignore @@ main @@ Uri.of_string !uri;
  Lwt_main.run (fst (Lwt.wait ()))
