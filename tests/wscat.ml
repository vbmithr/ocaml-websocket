open Websocket

let (>>=) = Lwt.bind
let (<?>) a b = Lwt.choose [a;b]
let (<&>) a b = Lwt.join [a;b]

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun content -> Lwt.wrap
      (fun () -> push (Some (Frame.of_string content)))
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
  if Array.length Sys.argv < 2 then
    (Printf.eprintf "Usage: %s [-s] uri\n" Sys.argv.(0); exit 1)
  if Sys.argv.(1) = "-s"
  then
    Lwt_main.run (
      server "8080" >>= fun _ -> client (Uri.of_string "ws://localhost:8080"))
  else
    Lwt_main.run (client (Uri.of_string Sys.argv.(1)))
