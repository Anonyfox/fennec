(* A minimal, typed HTTP/1.1 client for testing — one request per connection, raw Eio sockets,
   zero general-purpose HTTP machinery. Returns a structured response the assertion DSL inspects.

   Not a general-purpose HTTP library: no keep-alive, no redirects, no chunked decoding.
   Exactly what a test needs: send a request, read the full response, assert on it. Connects to
   the real [host] (DNS-resolved), so it works against remote servers, not just localhost.

   TLS: an [https] target is upgraded to TLS via tls-eio with an ACCEPT-ANY-CERT authenticator
   — the right default for testing your OWN server (self-signed localhost, a staging box), where
   you're checking behaviour, not validating a public CA. The handshake still happens, so a
   non-TLS server on an https URL fails clearly. *)

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

let header_value name resp =
  List.find_map (fun (k, v) -> if String.lowercase_ascii k = String.lowercase_ascii name then Some v else None) resp.headers

(* the RNG TLS needs, installed once on first use (HTTP-only runs never touch it) *)
let rng = lazy (Mirage_crypto_rng_unix.use_default ())

(* accept any certificate chain — testing your own server, not validating a CA *)
let accept_any : X509.Authenticator.t = fun ?ip:_ ~host:_ _certs -> Ok None

let tls_client =
  lazy (match Tls.Config.client ~authenticator:accept_any () with
        | Ok c -> c
        | Error (`Msg m) -> failwith ("fennec_hunt: TLS client config failed: " ^ m))

(* the common transport type: a read/write/shutdown flow. Both the raw TCP socket and the TLS
   flow coerce to this (dropping their extra capability tags), so [f] is transport-agnostic. *)
type flow = [ `R | `W | `Shutdown | `Flow ] Eio.Resource.t

(* Connect to [host]:[port], upgrade to TLS if [tls], run [f] on the resulting flow. *)
let with_connection ~net ~host ~port ~tls (f : flow -> 'a) : 'a =
  Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net (fun flow ->
      if not tls then f (flow :> flow)
      else begin
        Lazy.force rng;
        let peer = match Result.bind (Domain_name.of_string host) Domain_name.host with Ok h -> Some h | Error _ -> None in
        let tls_flow = Tls_eio.client_of_flow (Lazy.force tls_client) ?host:peer flow in
        f (tls_flow :> flow)
      end)

let request ~net ~host ~port ?(tls = false) ~meth ~path ?(headers = []) ?body () : response =
  let body = Option.value body ~default:"" in
  with_connection ~net ~host ~port ~tls @@ fun flow ->
  let has_host = List.exists (fun (k, _) -> String.lowercase_ascii k = "host") headers in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "%s %s HTTP/1.1\r\n" meth path);
  if not has_host then Buffer.add_string buf (Printf.sprintf "Host: %s:%d\r\n" host port);
  Buffer.add_string buf "Connection: close\r\n";
  List.iter (fun (k, v) -> Buffer.add_string buf (Printf.sprintf "%s: %s\r\n" k v)) headers;
  if body <> "" then Buffer.add_string buf (Printf.sprintf "Content-Length: %d\r\n" (String.length body));
  Buffer.add_string buf "\r\n";
  if body <> "" then Buffer.add_string buf body;
  Eio.Flow.copy_string (Buffer.contents buf) flow;
  (* half-close the write side to signal end-of-request — but NOT over TLS, where a close_notify
     makes many servers tear the whole connection down before we read the response. The request
     is self-delimiting anyway (Connection: close + Content-Length), so the hint is optional. *)
  if not tls then Eio.Flow.shutdown flow `Send;
  (* read the full response *)
  let raw = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  let status_line = Eio.Buf_read.line raw in
  let status =
    match String.split_on_char ' ' status_line with
    | _ :: code :: _ -> ( match int_of_string_opt code with Some c -> c | None -> 0)
    | _ -> 0
  in
  let rec read_headers acc =
    let line = Eio.Buf_read.line raw in
    if line = "" || line = "\r" then List.rev acc
    else
      match String.index_opt line ':' with
      | Some i ->
        let k = String.trim (String.sub line 0 i) in
        let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
        read_headers ((k, v) :: acc)
      | None -> read_headers acc
  in
  let headers = read_headers [] in
  (* the body is everything until EOF (we send Connection: close). One bulk read, not a
     char-at-a-time loop. *)
  let body = Eio.Buf_read.take_all raw in
  { status; headers; body }
