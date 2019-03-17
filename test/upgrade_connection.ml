open Lwt.Infix
open Websocket

let src = Logs.Src.create "websocket.upgrade_connection"

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let handler (_, conn) req body =
  let open Frame in
  Logs_lwt.app ~src begin fun m ->
    m "[CONN] %a" Sexplib.Sexp.pp (Cohttp.Connection.sexp_of_t conn)
  end >>= fun _ ->
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
    Logs_lwt.app ~src (fun m -> m "[PATH] /") >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:{|
        <html>
        <head>
            <meta charset="utf-8">
            <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
            <script>
                $(window).on('load', function(){
                    ws = new WebSocket('ws://localhost:7777/ws');
                    ws.onmessage = function(x) {
                        console.log(x.data);
                        var m = "<- Pong " + parseInt((x.data.substring(8)) - 1);
                        $('#msg').html("<p>" + x.data + "</p><p>" + m + "</p>");
                        ws.send(m);
                    };
                });
        </script>
        </head>
        <body>
            <div id='msg'></div>
        </body>
        </html>
        |} () >|= fun resp ->
    `Response resp
  | "/ws" ->
    Logs_lwt.app ~src (fun m -> m "[PATH] /ws") >>= fun () ->
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    Websocket_cohttp_lwt.upgrade_connection
      req begin fun { opcode ; content ; _ } ->
      match opcode with
      | Opcode.Close ->
        Logs.app ~src (fun m -> m "[RECV] CLOSE")
      | _ ->
        Logs.app ~src (fun m -> m "[RECV] %s" content)
    end >>= fun (resp, frames_out_fn) ->
    (* send a message to the client every second *)
    let num_ref = ref 10 in
    let rec go () =
      if !num_ref = 0 then
        Logs_lwt.app ~src (fun m -> m "[INFO] Test done")
      else
      let msg = Printf.sprintf "-> Ping %d" !num_ref in
      Logs_lwt.app ~src (fun m -> m "[SEND] %s" msg) >>= fun () ->
      Lwt.wrap1 frames_out_fn @@
      Some (Frame.create ~content:msg ()) >>= fun () ->
      decr num_ref ;
      Lwt_unix.sleep 1. >>=
      go
    in
    Lwt.async go ;
    Lwt.return resp
  | _ ->
    Logs_lwt.app ~src (fun m -> m "[PATH] Catch-all") >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Not_found
      ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
      () >|= fun resp ->
    `Response resp

let start_server ?tls port =
  let conn_closed (_,c) =
    Logs.app ~src begin fun m ->
      m "[SERV] connection %a closed" Sexplib.Sexp.pp
        (Cohttp.Connection.sexp_of_t c)
    end in
  Logs_lwt.app ~src begin fun m ->
    m "[SERV] Listening for HTTP on port %d" port
  end >>= fun () ->
  let mode = match tls with
  | None ->
    Logs.app ~src (fun m -> m "TCP mode selected") ;
    `TCP (`Port port)
  | Some (cert, key) ->
    Logs.app ~src (fun m -> m "TLS mode selected") ;
    `TLS (`Crt_file_path cert,
          `Key_file_path key,
          `No_password,
          `Port port) in
  Cohttp_lwt_unix.Server.create ~mode
    (Cohttp_lwt_unix.Server.make_response_action
       ~callback:handler ~conn_closed ())

let () =
  Logs.(set_reporter (lwt_reporter ())) ;
  let port = ref 7777 in
  let cert = ref "" in
  let key = ref "" in
  let speclist = Arg.align [
      "-cert", Arg.Set_string cert, " cert file";
      "-key", Arg.Set_string key, " key file";
      "-v", Arg.Unit (fun () ->
          Logs.set_level (Some Info)), " Set loglevel to info";
      "-vv", Arg.Unit (fun () ->
          Logs.set_level (Some Debug)), " Set loglevel to debug";
    ] in
  let anon_fun s =
    match int_of_string_opt s with
    | None -> invalid_arg "argument must be a port number"
    | Some p -> port := p in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> port\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  let tls = match !cert, !key with
  | "", _
  | _, "" -> None
  | cert, key -> Some (cert, key) in
  Lwt_main.run (start_server ?tls !port)
