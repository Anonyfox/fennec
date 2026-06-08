(* A minimal outbound HTTPS client for the ACME flow (talking to Let's Encrypt). The hunt client is
   test-only (the prod-lean guard forbids it in a server binary), so this is a small prod-safe client
   over the same tls-eio + x509 + ca-certs the server already links. One request per connection
   (Connection: close → read the body to EOF), which is plenty for ACME's handful of calls.

   Pure-OCaml TLS/crypto stack (no new native deps beyond what TLS termination already pulled). *)

type response = { status : int; headers : (string * string) list; body : string }

(* parse https://host[:port]/path → (host, port, path) *)
let parse_url url =
  let rest = match String.length url > 8 && String.sub url 0 8 = "https://" with true -> String.sub url 8 (String.length url - 8) | false -> failwith ("https_client: not an https URL: " ^ url) in
  let host_port, path = match String.index_opt rest '/' with Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i)) | None -> (rest, "/") in
  let host, port = match String.index_opt host_port ':' with Some i -> (String.sub host_port 0 i, int_of_string (String.sub host_port (i + 1) (String.length host_port - i - 1))) | None -> (host_port, 443) in
  (host, port, if path = "" then "/" else path)

let header_get headers k =
  let k = String.lowercase_ascii k in
  List.find_map (fun (h, v) -> if String.lowercase_ascii h = k then Some v else None) headers

(* the default authenticator verifies the peer against the OS trust store (ca-certs); a test/pebble
   caller passes its own (e.g. trusting pebble's CA, or X509.Authenticator.null) *)
let default_authenticator () =
  match Ca_certs.authenticator () with Ok a -> a | Error (`Msg m) -> failwith ("https_client: no system trust store — " ^ m)

let request ~net ?authenticator ~meth ?(headers = []) ?(body = "") url : response =
  Mirage_crypto_rng_unix.use_default ();
  let host, port, path = parse_url url in
  let authenticator = match authenticator with Some a -> a | None -> default_authenticator () in
  let cfg = Tls.Config.client ~authenticator () |> function Ok c -> c | Error (`Msg m) -> failwith ("https_client: TLS config — " ^ m) in
  let host_dn =
    match Domain_name.of_string host with
    | Ok d -> ( match Domain_name.host d with Ok h -> h | Error (`Msg m) -> failwith ("https_client: bad host — " ^ m))
    | Error (`Msg m) -> failwith ("https_client: bad host — " ^ m)
  in
  Eio.Switch.run @@ fun sw ->
  let addr = match Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host with a :: _ -> a | [] -> failwith ("https_client: cannot resolve " ^ host) in
  let raw = Eio.Net.connect ~sw net addr in
  let flow = Tls_eio.client_of_flow cfg ~host:host_dn raw in
  let req =
    Printf.sprintf "%s %s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\nContent-Length: %d\r\n%s\r\n%s" meth path host
      (String.length body)
      (String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers))
      body
  in
  Eio.Flow.copy_string req flow;
  let r = Eio.Buf_read.of_flow flow ~max_size:(8 * 1024 * 1024) in
  let status = match String.split_on_char ' ' (Eio.Buf_read.line r) with _ :: code :: _ -> int_of_string code | _ -> 0 in
  let rec hdrs acc =
    match Eio.Buf_read.line r with
    | "" -> List.rev acc
    | line -> ( match String.index_opt line ':' with Some i -> hdrs ((String.trim (String.sub line 0 i), String.trim (String.sub line (i + 1) (String.length line - i - 1))) :: acc) | None -> hdrs acc)
    | exception End_of_file -> List.rev acc
  in
  let headers = hdrs [] in
  let body = Eio.Buf_read.take_all r in
  { status; headers; body }
