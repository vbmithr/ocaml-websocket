open Lwt
open Websocket

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun content -> wrap
      (fun () -> push (Some {opcode=`Text; extension=0; final=true; content}))
      >>= read_fun in
    let rec write_fun () =
      Lwt_stream.next stream
      >>= fun { content } -> Lwt_io.printl content
      >>= write_fun in
    read_fun () <&> write_fun ()
  in
  with_connection uri cat_fun

let server port =
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream
    >>= fun frame -> wrap (fun () -> push (Some frame))
    >> echo_fun uri (stream, push) in
  lwt sockaddr = sockaddr_of_dns "localhost" port in
  establish_server sockaddr echo_fun

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [-s] uri\n" Sys.argv.(0);
  if Sys.argv.(1) = "-s"
  then
    Lwt_main.run (server "8080"
                  >>=
                    fun _ -> client (Uri.of_string "ws://localhost:8080"))
  else
    Lwt_main.run (client (Uri.of_string Sys.argv.(1)))
