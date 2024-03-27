open Websocket

val upgrade_connection :
  Cohttp_eio.Server.request ->
  (Frame.t -> unit) ->
  Cohttp_eio.Server.response * (Frame.t -> unit)
(** [upgrade_connection req incoming_handler] takes [req], a
        connection request, and [incoming_handler], a function that will
        process incoming websocket frames, and returns ([response_action],
        [push_frame]) where [response_action] is used to produce a
        {!Cohttp_lwt.Server.t} and [push_frame] is used to send websocket
        frames to the client. *)
