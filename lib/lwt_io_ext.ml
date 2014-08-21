open Lwt
open Lwt_io

let section = Lwt_log.Section.make "lwt_io_ext"

let trace sexp =
  Lwt_log.ign_debug_f "%s" (Sexplib.Sexp.to_string_hum sexp)

let open_connection ?tls_authenticator ?(host="") ?fd ?buffer_size sockaddr =
  let fd = match fd with
    | None -> Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    | Some fd -> fd
  in
  try_lwt
    match tls_authenticator with
    | None ->
      Lwt_unix.connect fd sockaddr >>= fun () ->
      (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
      (of_fd ?buffer_size ~mode:input fd,
       of_fd ?buffer_size ~mode:output fd)
      |> return

    | Some authenticator ->
      let config = Tls.Config.client ~authenticator () in
      Lwt_unix.connect fd sockaddr >>= fun () ->
      (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
      Tls_lwt.(Unix.client_of_fd ~trace ~host config fd >|= of_t)
  with exn ->
    (try_lwt Lwt_unix.close fd with _ -> return_unit) >>
    Lwt_log.warning ~exn ~section "Error opening connection" >>
    raise_lwt exn

let with_connection ?tls_authenticator ?fd ?buffer_size sockaddr f =
  lwt ic, oc = open_connection ?tls_authenticator ?fd ?buffer_size sockaddr in
  try_lwt
    f (ic, oc)
  finally
    close ic

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
  let abort_waiter = abort_waiter >> return `Shutdown in
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
          lwt () = Lwt_unix.close fd in
          match sockaddr with
          | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
            Unix.unlink path;
            return ()
          | _ ->
            return ()
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
          lwt () = Lwt_unix.close fd in
          match sockaddr with
          | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
            Unix.unlink path;
            return ()
          | _ ->
            return ()
      end
  in
  ignore (loop ());
  { shutdown = lazy(wakeup abort_wakener `Shutdown) }

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (match_lwt getaddrinfo node service
      [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] with
        | h::t -> return h
        | []   -> raise_lwt Not_found)
      >|= fun ai -> ai.ai_addr

let set_tcp_nodelay fd =
  Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true;
  if not (Lwt_unix.getsockopt fd Lwt_unix.TCP_NODELAY)
  then failwith "Unable to set TCP_NODELAY"
