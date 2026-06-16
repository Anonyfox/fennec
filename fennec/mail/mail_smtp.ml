type security = Plaintext | Starttls | Implicit_tls

type config = {
  host : string;
  port : int;
  security : security;
  user : string option;
  pass : string option;
  ehlo_domain : string;
}

let max_reply = 64 * 1024
let b64 = Base64.encode_string

(* the live connection: a read buffer + a write thunk, BOTH swappable so STARTTLS can replace the raw
   socket with the tls-eio flow mid-conversation (the two flows have different types, but [Buf_read.t]
   and [string -> unit] are monomorphic, so the swap is just two field writes) *)
type conn = { mutable r : Eio.Buf_read.t; mutable write : string -> unit }

let line conn s = conn.write (s ^ "\r\n")

(* read one reply, folding ESMTP multi-line continuations ("NNN-...") into the final ("NNN ...") code +
   the full text (used to sniff EHLO capabilities) *)
let reply conn =
  let buf = Buffer.create 128 in
  let rec loop code =
    let l = Eio.Buf_read.line conn.r in
    Buffer.add_string buf l;
    Buffer.add_char buf '\n';
    let code = if String.length l >= 3 then Option.value (int_of_string_opt (String.sub l 0 3)) ~default:code else code in
    if String.length l >= 4 && l.[3] = '-' then loop code else (code, Buffer.contents buf)
  in
  loop 0

exception Smtp of string

(* require a reply class (2 ⇒ 2xx success, 3 ⇒ 3xx intermediate like 354/334) *)
let need cls (code, _) ctx = if code / 100 <> cls then raise (Smtp (Printf.sprintf "%s: server replied %d" ctx code))

let contains hay sub =
  let hl = String.length hay and sl = String.length sub in
  let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
  sl = 0 || go 0

(* RFC 5321 §4.5.2 transparency: a leading '.' on ANY line is doubled, so a body line that happens to be
   "." cannot be read as the end-of-data marker *)
let dot_stuff data =
  let b = Buffer.create (String.length data + 16) in
  String.iteri
    (fun i c ->
      if c = '.' && (i = 0 || data.[i - 1] = '\n') then Buffer.add_char b '.';
      Buffer.add_char b c)
    data;
  Buffer.contents b

let ends_crlf s = let n = String.length s in n >= 2 && s.[n - 2] = '\r' && s.[n - 1] = '\n'

let tls_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg m) -> Error ("no system trust store: " ^ m)
  | Ok authenticator -> ( match Tls.Config.client ~authenticator () with Ok c -> Ok c | Error (`Msg m) -> Error ("TLS config: " ^ m))

let host_dn host =
  match Domain_name.of_string host with
  | Ok d -> ( match Domain_name.host d with Ok h -> Some h | Error _ -> None)
  | Error _ -> None

let send ~sw ~net config ~envelope_from ~recipients ~data =
  try
    Mirage_crypto_rng_unix.use_default ();
    let addr =
      match Eio.Net.getaddrinfo_stream ~service:(string_of_int config.port) net config.host with
      | a :: _ -> a
      | [] -> raise (Smtp ("cannot resolve host " ^ config.host))
    in
    let raw = Eio.Net.connect ~sw net addr in
    let conn = { r = Eio.Buf_read.of_flow ~max_size:max_reply raw; write = (fun s -> Eio.Flow.copy_string s raw) } in
    let upgrade () =
      match tls_config () with
      | Error m -> raise (Smtp m)
      | Ok cfg ->
        let tls = Tls_eio.client_of_flow cfg ?host:(host_dn config.host) raw in
        conn.r <- Eio.Buf_read.of_flow ~max_size:max_reply tls;
        conn.write <- (fun s -> Eio.Flow.copy_string s tls)
    in
    if config.security = Implicit_tls then upgrade ();
    need 2 (reply conn) "greeting";
    let ehlo () =
      line conn ("EHLO " ^ config.ehlo_domain);
      let r = reply conn in
      need 2 r "EHLO";
      String.uppercase_ascii (snd r)
    in
    let caps = ehlo () in
    (* opportunistic STARTTLS: upgrade when offered (a real submission server on 587); a local catcher
       that doesn't advertise it stays plaintext, which is what dev wants *)
    let caps =
      if config.security = Starttls && contains caps "STARTTLS" then (
        line conn "STARTTLS";
        need 2 (reply conn) "STARTTLS";
        upgrade ();
        ehlo ())
      else caps
    in
    (match (config.user, config.pass) with
    | Some u, Some p ->
      if contains caps "AUTH" && contains caps "PLAIN" then (
        line conn ("AUTH PLAIN " ^ b64 ("\000" ^ u ^ "\000" ^ p));
        need 2 (reply conn) "AUTH PLAIN")
      else if contains caps "AUTH" && contains caps "LOGIN" then (
        line conn "AUTH LOGIN";
        need 3 (reply conn) "AUTH LOGIN";
        line conn (b64 u);
        need 3 (reply conn) "AUTH LOGIN (username)";
        line conn (b64 p);
        need 2 (reply conn) "AUTH LOGIN (password)")
      else raise (Smtp "server does not offer AUTH PLAIN or LOGIN")
    | _ -> ());
    line conn ("MAIL FROM:<" ^ envelope_from ^ ">");
    need 2 (reply conn) "MAIL FROM";
    if recipients = [] then raise (Smtp "no recipients");
    List.iter
      (fun rcpt ->
        line conn ("RCPT TO:<" ^ rcpt ^ ">");
        need 2 (reply conn) ("RCPT TO " ^ rcpt))
      recipients;
    line conn "DATA";
    need 3 (reply conn) "DATA";
    let data = if ends_crlf data then data else data ^ "\r\n" in
    conn.write (dot_stuff data);
    conn.write ".\r\n";
    need 2 (reply conn) "end of DATA";
    (try
       line conn "QUIT";
       ignore (reply conn)
     with _ -> ());
    Ok ()
  with
  | Smtp m -> Error m
  | e -> Error (Printexc.to_string e)
