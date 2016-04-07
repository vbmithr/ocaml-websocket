val upgrade_connection:
  ?g:Nocrypto.Rng.g ->
  Cohttp.Request.t ->
  Conduit_lwt_unix.flow ->
  (Websocket.Frame.t -> unit) ->
  (Cohttp.Response.t * Cohttp_lwt_body.t * (Websocket.Frame.t option -> unit)) Lwt.t
