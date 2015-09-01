open Core.Std
open Async.Std

open Websocket_async

let log = Log.create ~level:`Error ~on_error:`Raise ~output:Log.Output.([stderr ()])

let client uri =
  let read_line_and_write_to_pipe w =
    let rec loop () =
      Reader.(read_line Lazy.(force stdin)) >>= function
      | `Eof ->
          Log.debug log "Got EOF. Closing pipe.";
          Pipe.close w;
          Shutdown.exit 0
      | `Ok s -> Pipe.write w s >>= loop
    in loop ()
  in
  client_ez uri >>= fun (r, w) ->
  don't_wait_for @@ read_line_and_write_to_pipe w;
  Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:(fun s -> s ^ "\n")

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
