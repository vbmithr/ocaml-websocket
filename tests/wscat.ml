open Lwt

let cat_fun (stream, push) =
  let rec read_fun () =
    Lwt_io.read_line Lwt_io.stdin
    >>= fun str -> wrap1 push (Some str)
    >>= read_fun in
  let rec write_fun () =
    Lwt_stream.next stream
    >>= Lwt_io.printl
    >>= write_fun in
  read_fun () <&> write_fun ()

let main uri =
  Websocket.with_connection uri cat_fun

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s uri\n" Sys.argv.(0)
  else Lwt_main.run (main (Uri.of_string Sys.argv.(1)))
