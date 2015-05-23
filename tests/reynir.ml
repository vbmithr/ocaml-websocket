open Websocket
open Lwt.Infix

let h = Hashtbl.create 17

let handler id uri send frame =
  (try
     Hashtbl.find h id
   with Not_found ->
     Hashtbl.add h id ();
     Format.printf "New connection (id = %d)@." id;
     Lwt.async (fun () ->
         Lwt_unix.sleep 1.0 >|= fun () ->
         send @@ Frame.create ~content:"Delayed message" ()
       )
  );
  Format.printf "<- %a@." Frame.pp frame;
  Some (Frame.create ~content:"OK" ())

let main port =
  establish_server (Unix.ADDR_INET (Unix.inet_addr_any, port)) handler

let () =
  let server_port = ref 14458 in

  let speclist = Arg.align
      [
        "-s", Arg.Set_int server_port, "<int> Run server on specified port";
        "-v", Arg.String (fun s -> Lwt_log.(add_rule s Info)), "<section> Put <section> to Info level";
        "-vv", Arg.String (fun s -> Lwt_log.(add_rule s Debug)), "<section> Put <section> to Debug level"
      ]
  in
  let anon_fun s = () in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;

  ignore @@ main !server_port;
  Lwt_main.run (fst (Lwt.wait ()))
