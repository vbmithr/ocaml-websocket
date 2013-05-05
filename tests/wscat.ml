open Websocket

let (>>=) = Lwt.bind
let (<?>) a b = Lwt.choose [a;b]
let (<&>) a b = Lwt.join [a;b]

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun content -> Lwt.wrap (fun () -> push (Some (Frame.of_string content)))
      >>= read_fun in
    let rec write_fun () =
      Lwt_stream.next stream
      >>= fun fr -> Lwt_io.printl (Frame.get_content fr)
      >>= write_fun in
    read_fun () <&> write_fun ()
  in
  with_connection uri cat_fun

let server port =
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream
    >>= fun frame -> Lwt.wrap (fun () -> push (Some frame))
    >> echo_fun uri (stream, push) in
  lwt sockaddr = sockaddr_of_dns "localhost" port in
  Lwt.return (establish_server sockaddr echo_fun)

let _ =
  let run_server port =
    server port >>= fun _ -> client (Uri.of_string ("ws://localhost:" ^ port)) in
  let server_port = ref "" in
  let endpoint_address = ref "" in

  Arg.parse [("-s", Arg.Set_string server_port, "Run server on specified port")]
    (fun s -> endpoint_address := s)
    "Usage: %s [-s port] uri\n";

  let main_thread =
    if !server_port <> "" then
      run_server !server_port
    else client (Uri.of_string !endpoint_address) in
  Lwt_main.run main_thread




