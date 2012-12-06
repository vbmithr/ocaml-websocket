open Lwt
open Websocket

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun str -> wrap (fun () -> push (Some str); push (Some ""))
      >>= read_fun in
    let rec write_fun () =
      Lwt_stream.next stream
      >>= (function "" -> Lwt_io.print "\n" | str -> Lwt_io.print str)
      >>= write_fun in
    read_fun () <&> write_fun ()
  in
  with_connection uri cat_fun

let server port =
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream
    >>= fun str ->
    wrap (fun () -> push (Some str); push (Some ""))
    >> echo_fun uri (stream, push) in
  lwt sockaddr = sockaddr_of_dns "localhost" port in
  establish_server sockaddr echo_fun

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [-s] uri\n" Sys.argv.(0);
  if Sys.argv.(1) = "-s"
  then
    Lwt_main.run (server Sys.argv.(2) >>= fun _ -> Lwt_unix.sleep 9999.)
  else
    Lwt_main.run (client (Uri.of_string Sys.argv.(1)))
