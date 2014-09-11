open Lwt
open Websocket

let client uri =
  let cat_fun (stream, push) =
    let rec read_fun () =
      Lwt_io.read_line Lwt_io.stdin >>= fun content ->
      Lwt.wrap (fun () -> push (Some (Frame.of_string ~content ())))
      >>= read_fun in
    let rec write_fun () =
      Lwt_stream.next stream >>= fun fr ->
      (match Frame.content fr with
       | None -> return_unit
       | Some content -> Lwt_io.printf "%s\n> " content)
      >>= write_fun in
    Lwt_io.printf "> " >>= fun () ->
    read_fun () <&> write_fun ()
  in
  with_connection uri cat_fun

let server ?certificate sockaddr =
  let rec echo_fun uri (stream, push) =
    Lwt_stream.next stream >>= fun frame ->
    Lwt.wrap (fun () -> push (Some frame)) >>= fun () ->
    echo_fun uri (stream, push) in
  establish_server ?certificate sockaddr echo_fun

let rec wait_forever () =
  Lwt_unix.sleep 1000.0 >>= wait_forever

let _ =
  let server_port = ref "" in
  let endpoint_address = ref "" in
  let cert_dir = ref None in

  let speclist = Arg.align
      [
        "-tls", Arg.String (fun s -> cert_dir := Some s), "<cert_dir> Use TLS for the server";
        "-s", Arg.Set_string server_port, "<int> Run server on specified port";
        "-v", Arg.String (fun s -> Lwt_log.(add_rule s Info)), "<section> Put <section> to Info level";
        "-vv", Arg.String (fun s -> Lwt_log.(add_rule s Debug)), "<section> Put <section> to Debug level"
      ]
  in
  let anon_fun s = endpoint_address := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;

  let main () =
    Tls_lwt.rng_init () >>= fun () ->
    match !server_port, !endpoint_address with
    | p, "" when p <> "" ->
      begin
        Lwt_io_ext.sockaddr_of_dns "localhost" !server_port >>= fun sa ->
        match !cert_dir with
        | None -> ignore (server sa); wait_forever ()
        | Some dir ->
          let cert = dir ^ "/server.crt" in
          let priv_key = dir ^ "/server.key" in
          X509_lwt.private_of_pems ~cert ~priv_key >>= fun certificate ->
          ignore (server ~certificate sa); wait_forever ()
      end
    | _, endpoint when endpoint <> "" ->
      client (Uri.of_string !endpoint_address)
    | _ -> Lwt_io.eprintl "Please specify a server port or a valid URI."
  in
  Lwt_main.run (main ())
