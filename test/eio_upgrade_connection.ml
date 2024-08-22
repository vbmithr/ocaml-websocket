open Eio.Std
open Websocket

let html_response s =
  Cohttp_eio.Server.respond_string ~status:`OK ~body:s ()

let handler ~sw con req _body : Cohttp_eio.Server.response_action =
  let open Frame in
  let uri = Http.Request.resource req in
  match uri with
  | "/" ->
      traceln "[PATH] /" ;
      let resp =
        html_response
          {|
        <html>
        <head>
            <meta charset="utf-8">
            <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
            <script>
                console.log("Hello World!");
                $(window).on('load', function(){
                    ws = new WebSocket('ws://localhost:7777/ws');
                    console.log(ws);
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
      in
      `Response resp
  | "/ws" ->
      traceln "[PATH] /ws" ;
      let resp, send_frame =
        Websocket_eio.upgrade_connection req (fun {opcode; content; _} ->
            match opcode with
            | Opcode.Close -> traceln "[RECV] CLOSE"
            | _ -> traceln "[RECV] %s" content ) in
      (* send a message to the client every second *)
      let num_ref = ref 10 in
      let rec go () =
        if !num_ref = 0 then traceln "[INFO] Test done"
        else
          let msg = Printf.sprintf "-> Ping %d" !num_ref in
          traceln "[SEND] %s" msg ;
          send_frame @@ Frame.create ~content:msg () ;
          decr num_ref ;
          Eio_unix.sleep 1. ;
          go () in
      Fiber.fork ~sw go ; resp
  | _ ->
      traceln "[PATH] Catch-all";
      `Response (Http.Response.make ~status:`Not_found (), Cohttp_eio.Body.of_string "")
      
let start_server env sw port =
  traceln "[SERV] Listening for HTTP on port %d" port ;
  let server = Cohttp_eio.Server.make_response_action ~callback:(handler ~sw) () in
  let socket = Eio.Net.listen ~sw ~backlog:5 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Cohttp_eio.Server.run ~port ~on_error:(Eio.traceln "%a" Fmt.exn) socket server 

let () =
  let port = ref 7777 in
  let cert = ref "" in
  let key = ref "" in
  let speclist =
    Arg.align
      [ ("-cert", Arg.Set_string cert, " cert file");
        ("-key", Arg.Set_string key, " key file");
        ( "-v",
          Arg.Unit (fun () -> Logs.set_level (Some Info)),
          " Set loglevel to info" );
        ( "-vv",
          Arg.Unit (fun () -> Logs.set_level (Some Debug)),
          " Set loglevel to debug" ) ] in
  let anon_fun s =
    match int_of_string_opt s with
    | None -> invalid_arg "argument must be a port number"
    | Some p -> port := p in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> port\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg ;
  Eio_main.run @@ fun env -> 
  Switch.run @@ fun sw -> 
  start_server env sw !port
