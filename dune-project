(lang dune 2.9)
(name websocket)

(generate_opam_files true)

(source (github vbmithr/ocaml-websocket))
(license ISC)
(authors "Vincent Bernardoff <vb@luminar.eu.org>")
(maintainers "Vincent Bernardoff <vb@luminar.eu.org>")
(documentation https://vbmithr.github.io/ocaml-websocket/doc)

(package
 (name websocket)
 (synopsis "Websocket library")
 (description
"\| The WebSocket Protocol enables two-way communication between a client
"\| running untrusted code in a controlled environment to a remote host
"\| that has opted-in to communications from that code.
"\|
"\| The security model used for this is the origin-based security model
"\| commonly used by web browsers. The protocol consists of an opening
"\| handshake followed by basic message framing, layered over TCP.
"\|
"\| The goal of this technology is to provide a mechanism for
"\| browser-based applications that need two-way communication with
"\| servers that does not rely on opening multiple HTTP connections (e.g.,
"\| using XMLHttpRequest or <iframe>s and long polling).
)
 (tags (org:mirage org:xapi-project))
 (depends
  (ocaml (>= 4.06.0))
  (base64 (>= 3.3.0))
  (conduit (>= 5.1.0))
  (cohttp (>= 5.0.0))
  (ocplib-endian (>= 1.0))
  (astring (>= 0.8.3))
  (mirage-crypto-rng (>= 1.0.0))))

(package
 (name websocket-lwt-unix)
 (synopsis "Websocket library (Lwt)")
 (description
"\| The WebSocket Protocol enables two-way communication between a client
"\| running untrusted code in a controlled environment to a remote host
"\| that has opted-in to communications from that code.
"\|
"\| The security model used for this is the origin-based security model
"\| commonly used by web browsers. The protocol consists of an opening
"\| handshake followed by basic message framing, layered over TCP.
"\|
"\| The goal of this technology is to provide a mechanism for
"\| browser-based applications that need two-way communication with
"\| servers that does not rely on opening multiple HTTP connections (e.g.,
"\| using XMLHttpRequest or <iframe>s and long polling).
)
 (tags (org:mirage org:xapi-project))
 (depends
  (ocaml (>= 4.06.0))
  (websocket (= :version))
  (lwt_log (>= 1.1.1))
  (cohttp-lwt-unix (>= 5.0.0))))
