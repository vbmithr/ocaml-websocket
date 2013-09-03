open Websocket

let (>>=) = Lwt.bind
let (<?>) a b = Lwt.choose [a;b]
let (<&>) a b = Lwt.join [a;b]

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin >>= fun content ->
      Lwt.wrap (fun () -> push (Some (Frame.of_string content)))
      >>= read_fun in
    let rec write_fun () =
      Lwt_stream.next stream >>= fun fr ->
      Lwt_io.printl (Frame.content fr)
      >>= write_fun in
    read_fun () <&> write_fun ()
  in
  with_connection uri cat_fun

let server sockaddr =
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream
    >>= fun frame -> Lwt.wrap (fun () -> push (Some frame))
    >> echo_fun uri (stream, push) in
  establish_server sockaddr echo_fun

let rec wait_forever () =
  Lwt_unix.sleep 1000.0 >>= wait_forever

let _ =
  let server_port = ref "" in
  let endpoint_address = ref "" in

  let run_server node service =
    Lwt_io_ext.sockaddr_of_dns node service >>= fun sa ->
    Lwt.return (server sa)
  in
  let speclist = Arg.align
      [ ("-s", Arg.Set_string server_port, " Run server on specified port");
      ]
  in
  let anon_fun s = endpoint_address := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  let main_thread =
    if !server_port <> "" then run_server "localhost" !server_port >>= fun _ -> wait_forever ()
    else client (Uri.of_string !endpoint_address) in
  Lwt_main.run main_thread




