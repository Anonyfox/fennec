(** Paw — a small, complete HTTP toolkit for OCaml/Eio. This module is the whole
    public surface; the implementation is organized in folders (http/, conn/,
    pipeline/, ws/, dev/). A paw touches a connection ([Conn.t -> Conn.t]); compose
    with {!seq}, the first to answer wins. *)

(* the paw algebra — a paw is [Conn.t -> Conn.t]; compose with [seq], the first to answer wins.
   The route/middleware verbs (get/use/…) below build {!Endpoint}s; the raw route-as-paw forms live
   under {!Route}. *)
type t = Pipeline.t

let seq = Pipeline.seq
let pass = Pipeline.pass
let run = Pipeline.run
let run_conn = Pipeline.run_conn
let fallthrough = Pipeline.fallthrough

(** {1 Route-as-paw primitives} *)

(* Build a single self-contained route — a paw answering one method+path, declining otherwise — for
   mounting (via {!use}) or composing. The {!Endpoint} verbs ({!get}/{!post}/…) are what an app
   reaches for; these are for reusable, mountable routes (what the accounts paws emit). *)
module Route = struct
  let on = Pipeline.on
  let get = Pipeline.get
  let post = Pipeline.post
  let put = Pipeline.put
  let delete = Pipeline.delete
  let patch = Pipeline.patch
end

(** {1 Serve — the one-call entry point} *)

(* [serve endpoints] = build the host router + run the Eio acceptor, owning the event loop — the
   [app.listen] of Paw. ?tls terminates TLS in-process from a loaded cert (SNI-selected per domain
   among the cert's SANs / several certs); ?acme manages Let's Encrypt certs automatically (per
   router domain, on-demand, renewed). With EITHER, production serves HTTPS on :443 and a :80 front
   redirects HTTP->HTTPS (and answers the ACME HTTP-01 challenge), so in-process HTTPS is transparent.
   A clashing route table or a busy port fails loudly. Drop to Host_router.build + Server.run for your
   own Eio env / prebuilt router. *)
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
  (* fail at boot on an ambiguous route table — a (method, exact path) declared twice — rather than
     silently shadowing the second handler at runtime *)
  (match List.concat_map Endpoint.conflicts endpoints with [] -> () | cs -> List.iter (fun c -> Printf.eprintf "paw: %s\n%!" c) cs; exit 1);
  match Host_router.build (List.map (fun e -> (Endpoint.name e, Endpoint.hosts e, e)) endpoints) with
  | Error errs ->
    prerr_endline (Host_router.describe_errors errs);
    exit 1
  | Ok router -> (
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let is_dev = try Sys.getenv Dev_proto.env_mode <> "production" with Not_found -> true in
    let net = Eio.Stdenv.net env in
    (* the ACME HTTP-01 token table; shared with the issuer when ~acme is set, empty for a BYO cert
       (then the :80 front is redirect-only). [tls_lock] guards it (and the issuer's cert tables)
       against concurrent access from the :80 front + worker-domain on-demand issuance. *)
    let challenges : (string, string) Hashtbl.t = Hashtbl.create 8 in
    let tls_lock = Mutex.create () in
    let tls_source, on_demand =
      match acme with
      | Some cfg ->
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
          Acme.run ~sw ~clock:(Eio.Stdenv.clock env) ~net ~lock:tls_lock ~domains ~challenges cfg
        in
        (Some source, on_demand)
      | None -> (Option.map (fun t () -> Some t) tls, None)
    in
    (* opt-in zero-config local HTTPS: in dev with no BYO/ACME cert, FENNEC_DEV_TLS=1 terminates TLS
       on the dev port with a throwaway self-signed cert for localhost (+ the endpoints' Exact hosts),
       so HTTPS-only behaviour (Secure cookies, HSTS, redirects) is testable locally with no cert to
       manage. Built once, presented as a constant source. *)
    let tls_source =
      match tls_source with
      | Some _ -> tls_source
      | None when is_dev && (match Sys.getenv_opt "FENNEC_DEV_TLS" with Some ("1" | "on" | "true" | "yes") -> true | _ -> false) ->
        let hosts =
          "localhost"
          :: List.concat_map (fun e -> List.filter_map (fun h -> match Host_pattern.of_string h with Ok (Host_pattern.Exact d) -> Some d | _ -> None) (Endpoint.hosts e)) endpoints
          |> List.sort_uniq compare
        in
        let cfg = Tls_termination.self_signed ~hosts () in
        Some (fun () -> Some cfg)
      | None -> None
    in
    (* in TLS-mode production the app is on :443; a :80 front 301-redirects HTTP→HTTPS (and serves the
       ACME HTTP-01 challenge from the shared table — empty, so redirect-only, for a BYO cert), so
       in-process HTTPS is transparent for both ACME and BYO. Dev keeps a single plain/forced port. *)
    if Option.is_some tls_source && not is_dev then Acme.serve_http_front ~sw ~net ~lock:tls_lock ~challenges;
    run ~env ~tls:tls_source ~on_demand router)

(* render framework errors (404/405/500/503) as JSON instead of plain text — for an API,
   [Paw.serve ~on_error:Paw.json_errors apps]. The default (plain text) needs no wiring. *)
let json_errors = Server.json_on_error

(** {1 The connection} *)
module Conn = Conn

module Assigns = Assigns

(** {1 Handler shortcuts} *)

(* The verbs reached for in nearly every paw, lifted to [Paw.] from {!Conn}. Reads pull a value OUT
   (conn-first); writes thread the connection THROUGH (conn-LAST), so a handler reads, then pipes the
   response — Elixir/Plug style:

     let create c =
       let title = Option.value (Paw.param c "title") ~default:"" in
       c |> Paw.set_status 201 |> Paw.set_cookie "sid" sid |> Paw.json (encode title)

   Reach for {!Conn} directly for the long tail (files, streaming, raw header lists, …). *)

(* reads — point-free aliases of {!Conn}, so go-to-definition + stack traces land there *)
let param = Conn.param
let query = Conn.query
let cookie = Conn.cookie
let header = Conn.req_header
let body_param = Conn.body_param
let file = Conn.file
let files = Conn.files

(* writes — conn-LAST, so they compose with [|>] *)
let set_status = Conn.set_status
let set_header k v c = Conn.set_header c k v

let set_cookie ?path ?domain ?max_age ?expires ?secure ?http_only ?same_site name value c =
  Conn.set_cookie c ?path ?domain ?max_age ?expires ?secure ?http_only ?same_site name value

let delete_cookie ?path ?domain name c = Conn.delete_cookie c ?path ?domain name
let respond resp c = Conn.respond c resp
let html ?status ?headers body c = Conn.html ?status ?headers c body
let json ?status ?headers body c = Conn.json ?status ?headers c body
let text ?status ?headers body c = Conn.text ?status ?headers c body
let redirect ?status url c = Conn.redirect ?status c url
let send_file ?content_type ?download ~path c = Conn.send_file c ?content_type ?download ~path ()
let download ?content_type ~filename body c = Conn.download c ?content_type ~filename body

(* Server-Sent Events with a labelled [push] for the common case (data events, optionally named) — no
   need to spell out {!Sse.data}. Reach for {!Sse.stream} directly for comment heartbeats / full control. *)
let sse (producer : push:(?event:string -> ?id:string -> ?retry:int -> string -> unit) -> unit) c =
  Sse.stream c (fun ~push -> producer ~push:(fun ?event ?id ?retry data -> push (Sse.data ?event ?id ?retry data)))

(** {1 The endpoint — flat-pipe app assembly}

    Start with [endpoint] and pipe middleware and routes onto it, one per line; hand the result(s) to
    {!serve}. These are {!Endpoint} verbs lifted to [Paw.] (the endpoint is the last argument, so they
    chain with [|>]); the raw route-as-paw forms are under {!Route}. *)

(* [endpoint ()] — an empty endpoint to pipe onto (config via [~name] / [~hosts]). *)
let endpoint ?(name = "app") ?(hosts = [ "*" ]) () = Endpoint.make ~name ~hosts ()

let use = Endpoint.use
let use_matched = Endpoint.use_matched
let prepend = Endpoint.prepend
let pipe = Endpoint.pipe
let pipe_matched = Endpoint.pipe_matched
let get = Endpoint.get
let post = Endpoint.post
let put = Endpoint.put
let delete = Endpoint.delete
let patch = Endpoint.patch
let form = Endpoint.form
let app = Endpoint.app

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

(** {1 Middleware battery}

    Prebuilt paws — the lego blocks. Each [make]s a {!t}; pipe the ones you want onto an endpoint. *)

(** {2 Sessions & security} *)
module Session = Session

module Csrf = Csrf
module Cors = Cors
module Security_headers = Security_headers
module Force_https = Force_https

(** {2 Authentication} *)
module Basic_auth = Basic_auth

module Bearer_auth = Bearer_auth
module Webhook = Webhook

(** {2 Traffic shaping & limits} *)
module Rate_limit = Rate_limit

module Body_limit = Body_limit

(** {2 Observability} *)

(* the structured per-request event — the shared vocabulary the logger/metrics/dev-wire all speak;
   captured once by the server (see [Server.run ~on_access]) *)
module Access = Access
module Logger = Logger

module Request_id = Request_id
module Metrics = Metrics
module Response_time = Response_time

(** {2 Request hygiene} — normalize the request before it reaches a route *)
module Method_override = Method_override

module Normalize_path = Normalize_path
module Trusted_proxy = Trusted_proxy
module Accepts = Accepts
module Ip_filter = Ip_filter

(** {2 Response shaping & assets} *)
module Static = Static

module Cache_control = Cache_control
module Set_header = Set_header
module Status_pages = Status_pages

(** {2 Operations} *)
module Health = Health

(** {1 Server-Sent Events} *)
module Sse = Sse

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
