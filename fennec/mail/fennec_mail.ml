module Address = Mail_address
module Smtp = Mail_smtp

type attachment = Mail_mime.attachment = { filename : string; content_type : string; content : string }

type t = {
  from : Address.t;
  to_ : Address.t list;
  cc : Address.t list;
  bcc : Address.t list;
  reply_to : Address.t option;
  subject : string;
  text : string option;
  html : string option;
  headers : (string * string) list;
  attachments : attachment list;
}

let make ~from ?(to_ = []) ?(cc = []) ?(bcc = []) ?reply_to ~subject ?text ?html ?(headers = [])
    ?(attachments = []) () =
  { from; to_; cc; bcc; reply_to; subject; text; html; headers; attachments }

type error = Invalid_message of string | Transport_error of string

let string_of_error = function
  | Invalid_message m -> "invalid message: " ^ m
  | Transport_error m -> "mail transport error: " ^ m

let to_mime m =
  Mail_mime.serialize ~from:m.from ~to_:m.to_ ~cc:m.cc ?reply_to:m.reply_to ~subject:m.subject ?text:m.text
    ?html:m.html ~headers:m.headers ~attachments:m.attachments ()

let recipients m = List.map Address.email (m.to_ @ m.cc @ m.bcc)

let validate m =
  if recipients m = [] then Error (Invalid_message "no recipients (to/cc/bcc all empty)")
  else if m.text = None && m.html = None then Error (Invalid_message "no text or html body")
  else Ok ()

type transport = t -> (unit, error) result

(* ---- built-in transports ---- *)

let log_transport ?(out = print_string) () : transport =
 fun m ->
  match validate m with
  | Error _ as e -> e
  | Ok () ->
    let body = match (m.text, m.html) with Some t, _ -> t | None, Some h -> h | None, None -> "" in
    let buf = Buffer.create 512 in
    Buffer.add_string buf "\n───── \xe2\x9c\x89 fennec mail (logged — MAIL_URL unset) ─────\n";
    Buffer.add_string buf ("From:    " ^ Address.to_header m.from ^ "\n");
    Buffer.add_string buf ("To:      " ^ Address.header_list m.to_ ^ "\n");
    if m.cc <> [] then Buffer.add_string buf ("Cc:      " ^ Address.header_list m.cc ^ "\n");
    if m.bcc <> [] then Buffer.add_string buf ("Bcc:     " ^ Address.header_list m.bcc ^ "\n");
    Buffer.add_string buf ("Subject: " ^ m.subject ^ "\n");
    if m.attachments <> [] then
      Buffer.add_string buf
        ("Attach:  " ^ String.concat ", " (List.map (fun a -> a.filename) m.attachments) ^ "\n");
    Buffer.add_string buf "\n";
    Buffer.add_string buf body;
    if m.text <> None && m.html <> None then Buffer.add_string buf "\n(— html alternative omitted from log —)";
    Buffer.add_string buf "\n──────────────────────────────────────────────\n";
    out (Buffer.contents buf);
    Ok ()

let capture () : transport * (unit -> t list) =
  let sent = ref [] in
  let tr m = match validate m with Error _ as e -> e | Ok () -> sent := m :: !sent; Ok () in
  (tr, fun () -> List.rev !sent)

let domain_of (a : Address.t) =
  match String.index_opt a.Address.email '@' with
  | Some i -> String.sub a.Address.email (i + 1) (String.length a.Address.email - i - 1)
  | None -> "localhost"

let smtp_transport ~sw ~net (config : Mail_smtp.config) : transport =
 fun m ->
  match validate m with
  | Error _ as e -> e
  | Ok () ->
    (* announce the sender's domain in EHLO unless the config pins one *)
    let ehlo_domain = if config.ehlo_domain = "" then domain_of m.from else config.ehlo_domain in
    let config = { config with ehlo_domain } in
    ( match
        Mail_smtp.send ~sw ~net config ~envelope_from:(Address.email m.from) ~recipients:(recipients m)
          ~data:(to_mime m)
      with
    | Ok () -> Ok ()
    | Error msg -> Error (Transport_error msg) )

(* ---- MAIL_URL → transport ---- *)

let starts pfx s = String.length s >= String.length pfx && String.sub s 0 (String.length pfx) = pfx

(* percent-decode a URL credential (passwords routinely carry %-encoded specials) *)
let pct_decode s =
  let hex c =
    match c with '0' .. '9' -> Char.code c - 48 | 'a' .. 'f' -> Char.code c - 87 | 'A' .. 'F' -> Char.code c - 55 | _ -> -1
  in
  let n = String.length s and b = Buffer.create (String.length s) in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '%' && i + 2 < n && hex s.[i + 1] >= 0 && hex s.[i + 2] >= 0 then (
      Buffer.add_char b (Char.chr ((hex s.[i + 1] * 16) + hex s.[i + 2]));
      go (i + 3))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* parse "smtp(s)://[user[:pass]@]host[:port]" → an Mail_smtp.config; None on any other/blank input *)
let parse_smtp_url url : Mail_smtp.config option =
  let security, default_port, rest =
    if starts "smtps://" url then (Mail_smtp.Implicit_tls, 465, String.sub url 8 (String.length url - 8))
    else if starts "smtp://" url then (Mail_smtp.Starttls, 587, String.sub url 7 (String.length url - 7))
    else (Mail_smtp.Plaintext, 0, "")
  in
  if default_port = 0 then None
  else begin
    let creds, hostport =
      match String.rindex_opt rest '@' with
      | Some i -> (Some (String.sub rest 0 i), String.sub rest (i + 1) (String.length rest - i - 1))
      | None -> (None, rest)
    in
    (* tolerate a trailing /path the way URLs allow it *)
    let hostport = match String.index_opt hostport '/' with Some i -> String.sub hostport 0 i | None -> hostport in
    let host, port =
      match String.rindex_opt hostport ':' with
      | Some i ->
        ( String.sub hostport 0 i,
          Option.value (int_of_string_opt (String.sub hostport (i + 1) (String.length hostport - i - 1))) ~default:default_port )
      | None -> (hostport, default_port)
    in
    let user, pass =
      match creds with
      | None -> (None, None)
      | Some c -> (
        match String.index_opt c ':' with
        | Some i -> (Some (pct_decode (String.sub c 0 i)), Some (pct_decode (String.sub c (i + 1) (String.length c - i - 1))))
        | None -> (Some (pct_decode c), None))
    in
    if host = "" then None else Some { Mail_smtp.host; port; security; user; pass; ehlo_domain = "" }
  end

let transport_of_env ~sw ~net () : transport =
  match Option.map String.trim (Sys.getenv_opt "MAIL_URL") with
  | Some url when url <> "" -> (
    match parse_smtp_url url with
    | Some config -> smtp_transport ~sw ~net config
    | None ->
      Printf.eprintf "fennec: MAIL_URL=%S is not an smtp:// or smtps:// URL — logging mail instead\n%!" url;
      log_transport ())
  | _ -> log_transport ()

(* ---- the ambient transport ---- *)

let _transport : transport option ref = ref None
let _hooks : (t -> bool) list ref = ref []
let _dev_capture : (t -> unit) option ref = ref None
let set_transport tr = _transport := Some tr
let on_before_send f = _hooks := f :: !_hooks
let set_dev_capture f = _dev_capture := Some f
let clear_dev_capture () = _dev_capture := None
let boot ~sw ~net () = set_transport (transport_of_env ~sw ~net ())

let send m =
  if not (List.for_all (fun h -> h m) !_hooks) then Ok () (* a before-send hook dropped it *)
  else (
    (* the dev outbox tees a copy of every accepted message here BEFORE the real transport runs, so the
       mailbox shows what was sent regardless of the transport (log/smtp). Never raises into [send]. *)
    (match !_dev_capture with Some f -> ( try f m with _ -> ()) | None -> ());
    match !_transport with Some tr -> tr m | None -> log_transport () m)

(* ---- inline tests (the socket-level SMTP test lives in test/ — it needs a real Eio loop) ---- *)

let%test "MAIL_URL parsing: smtp:// is STARTTLS:587, smtps:// is implicit-TLS:465, creds are decoded" =
  (match parse_smtp_url "smtp://user:p%40ss@mail.example.com" with
  | Some c -> c.security = Mail_smtp.Starttls && c.port = 587 && c.host = "mail.example.com" && c.user = Some "user" && c.pass = Some "p@ss"
  | None -> false)
  && (match parse_smtp_url "smtps://mail.example.com:2465" with
     | Some c -> c.security = Mail_smtp.Implicit_tls && c.port = 2465 && c.user = None
     | None -> false)
  && parse_smtp_url "https://nope" = None
  && parse_smtp_url "" = None

let%test "capture transport records sent messages; validate rejects no-recipient / no-body" =
  let tr, sent = capture () in
  let good = make ~from:(Address.v "a@x.com") ~to_:[ Address.v "b@x.com" ] ~subject:"s" ~text:"hi" () in
  let no_rcpt = make ~from:(Address.v "a@x.com") ~subject:"s" ~text:"hi" () in
  let no_body = make ~from:(Address.v "a@x.com") ~to_:[ Address.v "b@x.com" ] ~subject:"s" () in
  tr good = Ok ()
  && (match tr no_rcpt with Error (Invalid_message _) -> true | _ -> false)
  && (match tr no_body with Error (Invalid_message _) -> true | _ -> false)
  && List.length (sent ()) = 1

let%test "set_dev_capture tees a copy to the observer AND still runs the ambient transport" =
  let seen = ref [] in
  let delivered, sent = capture () in
  set_transport delivered;
  set_dev_capture (fun m -> seen := m :: !seen);
  let m = make ~from:(Address.v "a@x.com") ~to_:[ Address.v "b@x.com" ] ~subject:"s" ~text:"hi" () in
  let r = send m in
  clear_dev_capture ();
  set_transport delivered;
  (* the tee saw it AND the transport delivered it — both, not either *)
  r = Ok () && List.length !seen = 1 && List.length (sent ()) = 1

let%test "on_before_send returning false drops the message via the ambient send" =
  let tr, sent = capture () in
  set_transport tr;
  on_before_send (fun m -> m.subject <> "drop me");
  let mk subject = make ~from:(Address.v "a@x.com") ~to_:[ Address.v "b@x.com" ] ~subject ~text:"x" () in
  let kept = send (mk "keep") and dropped = send (mk "drop me") in
  kept = Ok () && dropped = Ok () && List.length (sent ()) = 1
