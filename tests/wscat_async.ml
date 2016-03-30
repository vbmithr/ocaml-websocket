open Core.Std
open Async.Std
open Log.Global

open Websocket_async

let client uri =
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri) in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let read_line_and_write_to_pipe w =
    let rec loop () =
      Reader.(read_line Lazy.(force stdin)) >>= function
      | `Eof ->
          debug "Got EOF. Closing pipe.";
          Pipe.close w;
          Shutdown.exit 0
      | `Ok s -> Pipe.write w s >>= loop
    in loop ()
  in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss"
     then Conduit_async_ssl.ssl_connect r w
     else return (r, w)) >>= fun (r, w) ->
    client_ez ~g:!Nocrypto.Rng.generator ~log:Lazy.(force log)
      ~heartbeat:Time.Span.(of_sec 5.) uri s r w >>= fun (r, w) ->
    don't_wait_for @@ read_line_and_write_to_pipe w;
    Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:(fun s -> s ^ "\n")
  in
  Tcp.(with_connection (to_host_and_port host port) tcp_fun)

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

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> anon ("url" %: string)
  in
  let set_loglevel = function
    | 2 -> set_level `Info
    | 3 -> set_level `Debug
    | _ -> ()
  in
  let run loglevel url =
    Option.iter loglevel ~f:set_loglevel;
    Nocrypto_entropy_unix.initialize ();
    don't_wait_for @@ client @@ Uri.of_string url;
    never_returns @@ Scheduler.go ()
  in
  Command.basic ~summary:"telnet-like interface to Websockets" spec run

let () = Command.run command
