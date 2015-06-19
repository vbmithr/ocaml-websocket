include Websocket
open Lwt.Infix

module Lwt_IO = IO(Cohttp_lwt_unix_io)
open Lwt_IO

let section = Lwt_log.Section.make "websocket_lwt"
exception HTTP_Error of string

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

(*c==v=[String.split_string]=1.2====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> if keep_empty then [""] else []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.2====*)

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

let with_connection ?(extra_headers = Cohttp.Header.init ()) ~ctx client uri =
  let connect () =
    let module C = Cohttp in
    let module CU = Cohttp_lwt_unix in
    let nonce = random_string ~base64:true 16 in
    let headers = C.Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = C.Request.make ~headers uri in
    Conduit_lwt_unix.(connect ~ctx:default_ctx client) >>= fun (flow, ic, oc) ->
    set_tcp_nodelay flow;
    let drain_handshake () =
      CU.Request.write (fun writer -> Lwt.return_unit) req oc >>= fun () ->
      CU.Response.read ic >>= (function
          | `Ok r -> Lwt.return r
          | `Eof -> Lwt.fail End_of_file
          | `Invalid s -> Lwt.fail @@ Failure s) >>= fun response ->
      let status = C.Response.status response in
      let headers = CU.Response.headers response in
      if C.Code.(is_error @@ code_of_status status)
      then Lwt.fail @@ HTTP_Error C.Code.(string_of_status status)
      else if not (C.Response.version response = `HTTP_1_1
                   && status = `Switching_protocols
                   && CCOpt.map String.lowercase @@
                   C.Header.get headers "upgrade" = Some "websocket"
                   && List.mem "upgrade" @@ List.map String.lowercase @@
                   C.Header.get_multi headers "connection"
                   && C.Header.get headers "sec-websocket-accept" =
                      Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
                  )
      then Lwt.fail_with "Protocol error"
      else Lwt_log.info_f ~section "Connected to %s" (Uri.to_string uri)
    in
    (try%lwt
      drain_handshake ()
     with exn ->
       Lwt_io.close ic >>= fun () ->
       Lwt.fail exn)
    >>= fun () ->
    Lwt.return (ic, oc)
  in
  connect () >|= fun (ic, oc) ->
  let read_frame = make_read_frame ~masked:true (ic, oc) in
  (fun () ->
     try%lwt
       read_frame ()
     with exn -> Lwt.fail exn),
  (fun frame ->
     try%lwt
       send_frame ~masked:true oc frame
     with exn -> Lwt.fail exn)

let headers_connection_contain_upgrade headers =
  let module C = Cohttp in
  let mem_upgrade s = strip_string (String.lowercase s) = "upgrade" in
  let pred str = List.exists mem_upgrade (split_string str [',']) in
  List.exists pred (C.Header.get_multi headers "connection")

let establish_server ?timeout ?stop ~ctx ~mode react =
  let module C = Cohttp in
  let module CU = Cohttp_lwt_unix in
  let id = ref @@ -1 in
  let server_fun id (ic, oc) =
    (CU.Request.read ic >>= function
      | `Ok r -> Lwt.return r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Lwt_log.info ~section "Remote endpoint closed connection" >>= fun () ->
        Lwt.fail End_of_file
      | `Invalid reason ->
        Lwt_log.info_f ~section "Invalid input from remote endpoint: %s" reason >>= fun () ->
        Lwt.fail @@ Failure reason) >>= fun request ->
    let meth    = C.Request.meth request in
    let version = C.Request.version request in
    let headers = C.Request.headers request in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && CCOpt.map String.lowercase @@
        C.Header.get headers "upgrade" = Some "websocket"
        && headers_connection_contain_upgrade headers
      )
    then Lwt.fail_with "Protocol error"
    else Lwt.return_unit >>= fun () ->
    let key = CCOpt.get_exn @@ C.Header.get headers "sec-websocket-key" in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers = C.Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = C.Response.make
        ~status:`Switching_protocols
        ~encoding:C.Transfer.Unknown
        ~headers:response_headers () in
    CU.Response.write (fun writer -> Lwt.return_unit) response oc >>= fun () ->
    let send_frame = send_frame ~masked:false oc in
    let read_frame = make_read_frame ~masked:false (ic, oc) in
    react id request read_frame send_frame
  in
  Lwt.async_exception_hook :=
    (fun exn -> Lwt_log.ign_warning ~section ~exn "async_exn_hook");
  Conduit_lwt_unix.serve ?timeout ?stop ~ctx ~mode
    (fun flow ic oc ->
       (try%lwt
         set_tcp_nodelay flow;
          incr id;
          server_fun !id (ic,oc)
        with
        | End_of_file ->
          Lwt_log.info ~section "Client closed connection"
        | Exit ->
          Lwt_log.info ~section "Server closed connection"
        | exn -> Lwt.fail exn
       ) [%finally Lwt_io.close ic]
    )

let mk_frame_stream recv =
  let f () =
    let%lwt fr = recv () in
    match fr.Frame.opcode with
    | Frame.Opcode.Close -> Lwt.return_none
    | _ -> Lwt.return (Some fr)
  in
  Lwt_stream.from f

let establish_standard_server ?timeout ?stop ~ctx ~mode react =
  let f id uri recv send =
  let f id req recv send =
    let recv fr =
      let%lwt fr = recv () in
      match fr.Frame.opcode with
      | Frame.Opcode.Ping ->
          send @@ Frame.create
            ~opcode:Frame.Opcode.Pong () >>= fun () -> Lwt.return fr
      | Frame.Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          (if String.length fr.Frame.content >= 2 then
             send @@ Frame.create ~opcode:Frame.Opcode.Close
               ~content:(String.sub fr.Frame.content 0 2) ()
           else send @@ Frame.close 1000
          ) >>= fun () -> Lwt.return fr

      | _ -> Lwt.return fr
    in
    react id uri recv send
  in
  establish_server ?timeout ?stop ~ctx ~mode f

