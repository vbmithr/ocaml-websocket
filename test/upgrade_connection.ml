open Lwt
open Websocket_cohttp_lwt

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  Lwt_io.eprintf
        "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
    Lwt_io.eprintf "[PATH] /\n%!"
    >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string
    ~status:`OK
    ~body: {|
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
        |}
    ()
  | "/ws" ->
    Lwt_io.eprintf "[PATH] /ws\n%!"
    >>= fun () ->
    Cohttp_lwt.Body.drain_body body
    >>= fun () ->
    Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
        fun f ->
            match f.opcode with
            | Opcode.Close ->
                Printf.eprintf "[RECV] CLOSE\n%!"
            | _ ->
                Printf.eprintf "[RECV] %s\n%!" f.content
    )
    >>= fun (resp, body, frames_out_fn) ->
    (* send a message to the client every second *)
    let _ =
        let num_ref = ref 10 in
        let rec go () =
            if !num_ref > 0 then
                let msg = Printf.sprintf "-> Ping %d" !num_ref in
                Lwt_io.eprintf "[SEND] %s\n%!" msg
                >>= fun () ->
                Lwt.wrap1 frames_out_fn @@
                    Some (Frame.create ~content:msg ())
                >>= fun () ->
                Lwt.return (num_ref := !num_ref - 1)
                >>= fun () ->
                Lwt_unix.sleep 1.
                >>= go
            else
                Lwt_io.eprintf "[INFO] Test done\n%!"
                >>= Lwt.return
        in
        go ()
    in
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ ->
    Lwt_io.eprintf "[PATH] Catch-all\n%!"
    >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string
        ~status:`Not_found
        ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
        ()

let start_server port =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:handler ~conn_closed ())

let () =
  Lwt_main.run (start_server 7777)
