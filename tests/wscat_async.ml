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
    let r, w = client_ez
        ~log:Lazy.(force log)
        ~heartbeat:Time.Span.(of_sec 5.) uri s r w
    in
    don't_wait_for @@ read_line_and_write_to_pipe w;
    Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:(fun s -> s ^ "\n")
  in
  Tcp.(with_connection (to_host_and_port host port) tcp_fun)

let server uri =
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri) in
  Tcp.Server.create (Tcp.on_port port) begin fun addr reader writer ->
    let app_to_ws, sender_write = Pipe.create () in
    let receiver_read, ws_to_app = Pipe.create () in
    let inet_addr = match addr with
      | `Inet _ as a -> a
    in
    let finished = Websocket_async.server ~app_to_ws ~ws_to_app ~reader ~writer inet_addr in
    let send = Pipe.write sender_write in
    let rec loop () =
      Pipe.read receiver_read >>= function
      | `Eof -> Deferred.unit
      | `Ok fr ->
        begin
          let open Frame in
          match fr.opcode with
          | Opcode.Ping ->
              send @@
              Frame.create ~opcode:Opcode.Pong ~content:fr.content () >>= fun () ->
              (* Immediately echo and pass this last message to the user *)
              if String.length fr.content >= 2 then
                send @@ Frame.create ~opcode:Opcode.Close
                  ~content:(String.sub fr.content 0 2) ()
              else
                send @@ Frame.close 100
          | Opcode.Pong -> Deferred.unit
          | Opcode.Text
          | Opcode.Binary ->
              send fr
          | _ ->
              send @@ Frame.close 1002
        end >>=
        loop
    in
    Deferred.any [loop (); finished]
  end >>| ignore

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> flag "-s" no_arg ~doc:"is-server Run as server, default: no"
    +> anon ("url" %: string)
  in
  let set_loglevel = function
    | 2 -> set_level `Info
    | 3 -> set_level `Debug
    | _ -> ()
  in
  let run loglevel is_server url =
    Option.iter loglevel ~f:set_loglevel;
    Nocrypto_entropy_unix.initialize ();
    don't_wait_for @@ (if is_server then server else client) @@ Uri.of_string url;
    never_returns @@ Scheduler.go ()
  in
  Command.basic ~summary:"telnet-like interface to Websockets" spec run

let () = Command.run command
