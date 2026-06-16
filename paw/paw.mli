(** Paw — a small, complete HTTP toolkit for OCaml/Eio. *)

include module type of Pipeline

module Conn = Conn
module Assigns = Assigns
module Http = Http
module Headers = Headers
module Cookie = Cookie
module Mime = Mime
module Multipart = Multipart
module Http_date = Http_date
module Http_semantics = Http_semantics
module Ws_channel = Ws_channel
module Dev = Dev
module Dev_proto = Dev_proto

(** {1 The Eio runtime} *)
module Server = Server

module Responder = Responder
module Gzip = Gzip
module Port_plan = Port_plan

(** {1 Routing & virtual hosts} *)
module Endpoint = Endpoint

module Host_router = Host_router
module Host_pattern = Host_pattern

(** {1 Middleware battery} *)
module Session = Session

module Csrf = Csrf
module Cors = Cors
module Static = Static
module Logger = Logger
module Rate_limit = Rate_limit
module Basic_auth = Basic_auth
module Force_https = Force_https
module Security_headers = Security_headers
module Request_id = Request_id
module Metrics = Metrics
module Method_override = Method_override

(** {1 WebSockets} *)
module Ws = Ws

module Websocket = Websocket

(** {1 TLS & automatic HTTPS} *)
module Tls_termination = Tls_termination

module Sni = Sni
module Cert_store = Cert_store
module Acme = Acme
module Https_client = Https_client

(** The ACME protocol client behind {!Acme}. Exposed for the pebble e2e wire test; most
    consumers want {!Acme} (the managed, on-demand certificate source). *)
module Acme_client = Acme_client

(** {1 Dev} *)
module Livereload = Livereload
