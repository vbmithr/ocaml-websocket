open Lwt
open Lwt_io

let shutdown_and_close_socket fd =
  try_lwt
    Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
    return ()
  with _ -> return ()
     finally
       try_lwt Lwt_unix.close fd with _ -> return ()

let open_connection ?setup_socket ?buffer_size sockaddr =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  (match setup_socket with Some f -> f fd | None -> ());
  try_lwt
    lwt () = Lwt_unix.connect fd sockaddr in
    (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
    return (of_fd ?buffer_size
        ~close:(fun _ -> shutdown_and_close_socket fd)
        ~mode:input fd,
      of_fd ?buffer_size
        ~close:(fun _ -> shutdown_and_close_socket fd)
        ~mode:output fd)
  with exn ->
    shutdown_and_close_socket fd >> raise_lwt exn

let with_connection ?setup_socket ?buffer_size sockaddr f =
  lwt ic, oc = open_connection ?setup_socket ?buffer_size sockaddr in
  try_lwt
    f (ic, oc)
  finally
    close ic <&> close oc

type server = { shutdown : unit Lazy.t }

let establish_server ?setup_server_socket ?setup_clients_sockets ?buffer_size ?(backlog=5) sockaddr f =
  let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
    (match setup_server_socket with Some f -> f sock | None -> ());
    Lwt_unix.bind sock sockaddr;
    Lwt_unix.listen sock backlog;
    let abort_waiter, abort_wakener = wait () in
    let abort_waiter = abort_waiter >> return `Shutdown in
    let rec loop () =
      pick [Lwt_unix.accept sock >|= (fun x -> `Accept x); abort_waiter] >>= function
      | `Accept(fd, addr) ->
        (match setup_clients_sockets with Some f -> f fd | None -> ());
        (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
        f (of_fd ?buffer_size ~mode:input ~close:(fun () -> shutdown_and_close_socket fd) fd,
          of_fd ?buffer_size ~mode:output ~close:(fun () -> shutdown_and_close_socket fd) fd);
        loop ()
      | `Shutdown ->
        lwt () = Lwt_unix.close sock in
        match sockaddr with
        | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
          Unix.unlink path;
          return ()
        | _ ->
          return ()
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
