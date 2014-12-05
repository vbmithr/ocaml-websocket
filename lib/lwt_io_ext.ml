open Lwt
open Lwt_io

let section = Lwt_log.Section.make "lwt_io_ext"

let trace sexp =
  Lwt_log.ign_debug_f "%s" (Sexplib.Sexp.to_string_hum sexp)

let safe_close_fd fd =
  catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> return_unit)

let safe_close ic =
  catch
    (fun () -> Lwt_io.close ic)
    (fun _ -> return_unit)

let open_connection ?tls_authenticator ?(host="") ?fd ?buffer_size sockaddr =
  let fd = match fd with
    | None -> Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    | Some fd -> fd in
  try%lwt match tls_authenticator with
    | None ->
        Lwt_unix.connect fd sockaddr >|= fun () ->
        (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
        (of_fd ?buffer_size ~mode:input fd,
         of_fd ?buffer_size ~mode:output fd)
    | Some authenticator ->
        let config = Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported ()) in
        Lwt_unix.connect fd sockaddr >>= fun () ->
        (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
        Tls_lwt.(Unix.client_of_fd ~trace ~host config fd >|= of_t)
  with exn ->
    safe_close_fd fd >>= fun () ->
    Lwt_log.warning ~exn ~section "Error opening connection" >>= fun () ->
    fail exn

let with_connection ?tls_authenticator ?fd ?buffer_size sockaddr f =
  open_connection ?tls_authenticator ?fd ?buffer_size sockaddr >>= fun (ic, oc) ->
  finalize (fun () -> f (ic, oc)) (fun () -> close ic)

type server = { shutdown : unit Lazy.t }

let establish_server ?certificate ?fd ?setup_clients_sockets ?buffer_size ?(backlog=5) sockaddr f =
  let fd = match fd with
    | None -> Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    | Some fd -> fd
  in
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd backlog;
  let abort_waiter, abort_wakener = wait () in
  let abort_waiter = abort_waiter >>= fun () -> return `Shutdown in
  let rec loop () =
    match certificate with
    | None ->
        begin
          pick [Lwt_unix.accept fd >|= (fun x -> `Accept x); abort_waiter] >>= function
          | `Accept(fd, _) ->
              (match setup_clients_sockets with Some f -> f fd | None -> ());
              (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
              Lwt.async (fun () -> f (of_fd ?buffer_size ~mode:input fd,
                                      of_fd ?buffer_size ~mode:output fd));
              loop ()
          | `Shutdown ->
              Lwt_unix.close fd >>= fun () ->
              match sockaddr with
              | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
                  Unix.unlink path;
                  return_unit
              | _ ->
                  return_unit
        end
    | Some certificate ->
        let tls_config = Tls.Config.server ~certificate () in
        begin
          pick [Lwt_unix.accept fd >|= (fun x -> `Accept x); abort_waiter] >>= function
          | `Accept(fd, _) ->
              (match setup_clients_sockets with Some f -> f fd | None -> ());
              (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
              Lwt.async (fun () -> Tls_lwt.(Unix.server_of_fd tls_config fd >|= of_t) >>= f);
              loop ()
          | `Shutdown ->
              Lwt_unix.close fd >>= fun () ->
              match sockaddr with
              | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
                  Unix.unlink path;
                  return_unit
              | _ ->
                  return_unit
        end
  in
  ignore (loop ());
  { shutdown = lazy(wakeup abort_wakener ()) }

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (getaddrinfo node service [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] >>= function
  | h::t -> return h
  | []   -> fail Not_found)
  >|= fun ai -> ai.ai_addr

let set_tcp_nodelay fd =
  Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true;
  if not (Lwt_unix.getsockopt fd Lwt_unix.TCP_NODELAY)
  then failwith "Unable to set TCP_NODELAY"
