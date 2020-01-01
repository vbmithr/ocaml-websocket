open Core
open Async
open Websocket_async

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ())

let client protocol extensions uri =
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port =
    match Uri.port uri, Uri_services.tcp_port_of_uri uri with
    | Some p, _ -> p
    | None, Some p -> p
    | _ -> invalid_arg "port cannot be inferred from URL" in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let tcp_fun (r, w) =
    let module C = Cohttp in
    let extra_headers = C.Header.init () in
    let extra_headers = Option.value_map protocol ~default:extra_headers
        ~f:(fun proto -> C.Header.add extra_headers "Sec-Websocket-Protocol" proto)
    in
    let extra_headers = Option.value_map extensions ~default:extra_headers
        ~f:(fun exts -> C.Header.add extra_headers "Sec-Websocket-Extensions" exts)
    in
    let r, w = client_ez
        ~extra_headers
        ~heartbeat:Time_ns.Span.(of_int_sec 5) uri r w
    in
    Deferred.all_unit [
      Pipe.transfer Reader.(pipe @@ Lazy.force stdin) w ~f:begin fun s ->
        String.chop_suffix_exn s ~suffix:"\n"
      end;
      Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:(fun s -> s ^ "\n")
    ]
  in
  Unix.Addr_info.get ~service:(string_of_int port) ~host [] >>= function
  | [] -> failwithf "DNS resolution failed for %s" host ()
  | { ai_addr; _ } :: _ ->
    let addr =
      match scheme, ai_addr with
      | _, ADDR_UNIX path -> `Unix_domain_socket path
      | "https", ADDR_INET (h, p)
      | "wss", ADDR_INET (h, p) ->
        let h = Ipaddr_unix.of_inet_addr h in
        `OpenSSL (h, p, Conduit_async.V2.Ssl.Config.create ())
      | _, ADDR_INET (h, p) ->
        let h = Ipaddr_unix.of_inet_addr h in
        `TCP (h, p)
    in
    Conduit_async.V2.connect addr >>= tcp_fun

let src = Logs.Src.create "websocket.wscat"

let handle_client addr reader writer =
  let addr_str = Socket.Address.(to_string addr) in
  Logs_async.info ~src begin fun m ->
    m "Client connection from %s" addr_str
  end >>= fun () ->
  let app_to_ws, sender_write = Pipe.create () in
  let receiver_read, ws_to_app = Pipe.create () in
  let check_request req =
    Logs_async.info ~src begin fun m ->
      m "Incoming connnection request: %a" Cohttp.Request.pp_hum req
    end >>= fun () ->
    Deferred.return (String.equal Cohttp.Request.(uri req |> Uri.path) "/ws")
  in
  let rec loop () =
    Pipe.read receiver_read >>= function
    | `Eof ->
      Logs_async.info ~src begin fun m ->
        m "Client %s disconnected" addr_str
      end
    | `Ok ({ Frame.opcode; content; _ } as frame) ->
      let open Frame in
      Logs_async.debug ~src (fun m -> m "<- %a" Frame.pp frame) >>= fun () ->
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          if String.length content >= 2 then
            Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content ~pos:0 ~len:2) ()), true
          else
          Some (close 100), true
        | Opcode.Pong -> None, false
        | Opcode.Text
        | Opcode.Binary -> Some frame, false
        | _ -> Some (close 1002), false
      in
      begin
        match frame' with
        | None ->
          Deferred.unit
        | Some frame' ->
          Logs_async.debug ~src (fun m -> m "-> %a" pp frame') >>= fun () ->
          Pipe.write sender_write frame'
      end >>= fun () ->
      if closed then Deferred.unit
      else loop ()
  in
  Deferred.any [
    begin server
        ~check_request ~app_to_ws ~ws_to_app ~reader ~writer () >>= function
      | Error err when Poly.equal (Error.to_exn err) Exit -> Deferred.unit
      | Error err -> Error.raise err
      | Ok () -> Deferred.unit
    end ;
    loop () ;
  ]

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-protocol" (optional string) ~doc:"str websocket protocol header"
    +> flag "-extensions" (optional string) ~doc:"str websocket extensions header"
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> flag "-s" no_arg ~doc:" Run as server (default: no)"
    +> anon ("url" %: string)
  in
  let set_loglevel = function
    | 2 -> Logs.set_level (Some Info)
    | 3 -> Logs.set_level (Some Debug)
    | _ -> ()
  in
  let run protocol extension loglevel is_server url () =
    Option.iter loglevel ~f:set_loglevel;
    let url = Uri.of_string url in
    match is_server with
    | false -> client protocol extension url
    | true ->
      let port = Option.value_exn
          ~message:"no port inferred from scheme"
          Uri_services.(tcp_port_of_uri url) in
      Tcp.(Server.create
             ~on_handler_error:`Ignore
             Where_to_listen.(of_port port) handle_client) >>=
      Tcp.Server.close_finished
  in
  Command.async_spec ~summary:"telnet-like interface to Websockets" spec run

let () = Command.run command
