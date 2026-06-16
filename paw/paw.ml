(** Paw — a small, complete HTTP toolkit for OCaml/Eio. This module is the whole
    public surface; the implementation is organized in folders (http/, conn/,
    pipeline/, ws/, dev/). A paw touches a connection ([Conn.t -> Conn.t]); compose
    with {!seq}, the first to answer wins. *)

include Pipeline

(** {1 Serve — the one-call entry point} *)

(* [serve endpoints] = build the host router + run the Eio acceptor, owning the event loop — the
   [app.listen] of Paw. ?tls terminates TLS from a loaded cert; ?acme manages Let's Encrypt certs
   automatically (HTTP-01 challenge + an HTTP->HTTPS front on :80). A clashing route table or a busy
   port fails loudly. Drop to Host_router.build + Server.run for your own Eio env / prebuilt router. *)
let serve ?tls ?acme ?on_error ?on_listen endpoints =
  let run ~env ~tls ~on_demand router =
    match Server.run ?tls ?on_demand ?on_error ?on_listen ~env router with
    | Ok () -> ()
    | Error (`Port_in_use port) ->
      Printf.eprintf "paw: port %d is already in use\n%!" port;
      exit 1
    | Error (`Bad_plan msg) ->
      Printf.eprintf "paw: %s\n%!" msg;
      exit 1
  in
  match Host_router.build (List.map (fun e -> (Endpoint.name e, Endpoint.hosts e, e)) endpoints) with
  | Error errs ->
    prerr_endline (Host_router.describe_errors errs);
    exit 1
  | Ok router -> (
    Eio_main.run @@ fun env ->
    match acme with
    | Some cfg ->
      Eio.Switch.run @@ fun sw ->
      let challenges : (string, string) Hashtbl.t = Hashtbl.create 8 in
      (* certify the concrete hosts the router answers; wildcards too once a DNS provider is set *)
      let derived =
        List.concat_map
          (fun e ->
            List.filter_map
              (fun h ->
                match Host_pattern.of_string h with
                | Ok (Host_pattern.Exact d) -> Some d
                | Ok (Host_pattern.Suffix s) when Acme.dns_enabled cfg -> Some ("*" ^ s)
                | _ -> None)
              (Endpoint.hosts e))
          endpoints
        |> List.sort_uniq compare
      in
      let domains = match Acme.domains_override cfg with Some d -> d | None -> derived in
      let ({ source; on_demand } : Acme.running) =
        Acme.run ~sw ~clock:(Eio.Stdenv.clock env) ~net:(Eio.Stdenv.net env) ~domains ~challenges cfg
      in
      Acme.serve_http_front ~sw ~net:(Eio.Stdenv.net env) ~challenges;
      run ~env ~tls:(Some source) ~on_demand router
    | None ->
      let tls = Option.map (fun t () -> Some t) tls in
      run ~env ~tls ~on_demand:None router)

(** {1 The connection} *)
module Conn = Conn

module Assigns = Assigns

(** {1 HTTP vocabulary} *)
module Http = Http

module Headers = Headers
module Cookie = Cookie
module Mime = Mime
module Multipart = Multipart
module Http_date = Http_date
module Http_semantics = Http_semantics

(** {1 WebSockets & dev} *)
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
