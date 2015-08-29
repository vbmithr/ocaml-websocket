open Core.Std
open Async.Std

open Websocket_async

let log = Log.create ~level:`Error ~on_error:`Raise ~output:Log.Output.([stderr ()])

let client uri =
  let react app_to_ws fr =
    let open Frame in
    match fr.opcode with
    | Opcode.Ping ->
      Pipe.write app_to_ws @@ Frame.create ~opcode:Opcode.Pong ()
    | Opcode.Close ->
      (* Immediately echo and pass this last message to the user *)
      (if String.length fr.content >= 2 then
        Pipe.write app_to_ws @@ Frame.create ~opcode:Opcode.Close
          ~content:(String.sub fr.content 0 2) ()
       else Pipe.write app_to_ws @@ Frame.close 1000) >>| fun () ->
      Pipe.close app_to_ws

    | Opcode.Pong -> Deferred.unit
    | Opcode.Text
    | Opcode.Binary ->
      Writer.(writef (Lazy.force stdout) "> %s\n> %!" fr.content);
      Deferred.unit
    | _ ->
      Pipe.write app_to_ws @@ Frame.close 1002 >>| fun () ->
      raise Exit
  in
  let read_line_and_write_to_pipe w =
    let rec loop () =
      Reader.(read_line Lazy.(force stdin)) >>= function
        | `Eof ->
          Log.debug log "Got EOF. Sending a close frame";
          Pipe.write w @@ Frame.close 1000 >>= fun () ->
          Shutdown.exit 0
        | `Ok content ->
          Pipe.write w @@ Frame.create ~content () >>= loop
    in loop ()
  in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri) in
  Tcp.(with_connection (to_host_and_port host port)
         (fun s r w ->
            Socket.(setopt s Opt.nodelay true);
            (if scheme = "https" || scheme = "wss"
            then Conduit_async_ssl.ssl_connect r w
            else return (r, w)) >>= fun (r, w) ->
            Log.info log "Connected to %s" @@ Uri.to_string uri;
            Writer.(writef (Lazy.force stdout) "> ");
            let client_read, ws_to_app = Pipe.create () in
            let app_to_ws, client_write = Pipe.create () in
            let net_to_ws = r in
            let ws_to_net = w in
            don't_wait_for @@ read_line_and_write_to_pipe client_write;
            don't_wait_for @@ Pipe.iter client_read (react client_write);
            client ~app_to_ws ~ws_to_app ~net_to_ws ~ws_to_net uri
         )
      )

(* let server uri = *)
(*   let echo_fun id req recv send = *)
(*     let open Frame in *)
(*     let react fr = *)
(*       match fr.opcode with *)
(*       | Opcode.Ping -> *)
(*         send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content () *)

(*       | Opcode.Close -> *)
(*         (\* Immediately echo and pass this last message to the user *\) *)
(*         (if String.length fr.content >= 2 then *)
(*            send @@ Frame.create ~opcode:Opcode.Close *)
(*              ~content:(String.sub fr.content 0 2) () *)
(*          else send @@ Frame.close 1000) >>= fun () -> *)
(*         Lwt.fail Exit *)

(*       | Opcode.Pong -> Lwt.return_unit *)

(*       | Opcode.Text *)
(*       | Opcode.Binary -> send fr *)

(*       | _ -> *)
(*         send @@ Frame.close 1002 >>= fun () -> *)
(*         Lwt.fail Exit *)
(*     in *)
(*     let rec react_forever () = recv () >>= react >>= react_forever *)
(*     in *)
(*     react_forever () *)
(*   in *)
(*   Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp -> *)
(*   Conduit_lwt_unix.(endp_to_server ~ctx:default_ctx endp >>= fun server -> *)
(*   establish_server ~ctx:default_ctx ~mode:server echo_fun) *)

let () =
  let uri = ref "" in
  let speclist = Arg.(align
      [
        "-v", Unit (fun () ->
            Log.set_level log `Info;
            Log.set_level Websocket_async.log `Info
          ), "Set log level to `Info";
        "-vv", Unit (fun () ->
            Log.set_level log `Debug;
            Log.set_level Websocket_async.log `Debug
          ), "Set log level to `Debug";
      ])
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  don't_wait_for @@ client @@ Uri.of_string !uri;
  never_returns @@ Scheduler.go ()
