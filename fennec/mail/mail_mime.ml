module A = Mail_address

type attachment = { filename : string; content_type : string; content : string }

let crlf = "\r\n"

(* base64, wrapped at 76 columns per line (the MIME line-length limit) *)
let b64 s =
  let e = Base64.encode_string s in
  let n = String.length e in
  let buf = Buffer.create (n + (n / 76 * 2) + 8) in
  let rec go i =
    if i >= n then ()
    else (
      let k = min 76 (n - i) in
      Buffer.add_substring buf e i k;
      Buffer.add_string buf crlf;
      go (i + k))
  in
  go 0;
  Buffer.contents buf

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

(* RFC 2047 'B' encoded-word for a non-ASCII Subject; plain ASCII passes through *)
let encode_subject s = if is_ascii s then s else "=?UTF-8?B?" ^ Base64.encode_string s ^ "?="

(* RFC 5322 date, always emitted in UTC (+0000) *)
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let wdays = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let date_header now =
  let tm = Unix.gmtime now in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d +0000" wdays.(tm.Unix.tm_wday) tm.Unix.tm_mday
    months.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* a unique-enough Message-ID — uniqueness, not unpredictability, so no CSPRNG needed *)
let seq = Atomic.make 0

let message_id domain =
  Printf.sprintf "<%.0f.%d.%d@%s>" (Unix.gettimeofday () *. 1e6) (Unix.getpid ()) (Atomic.fetch_and_add seq 1)
    domain

let domain_of (a : A.t) =
  match String.index_opt a.A.email '@' with
  | Some i -> String.sub a.A.email (i + 1) (String.length a.A.email - i - 1)
  | None -> "localhost"

(* ---- MIME entity model: a part is its headers + an already-encoded body, rendered uniformly ---- *)

type entity = { headers : (string * string) list; body : string }

let render { headers; body } = String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ crlf) headers) ^ crlf ^ body

let leaf ~content_type ?disposition body =
  { headers =
      [ ("Content-Type", content_type); ("Content-Transfer-Encoding", "base64") ]
      @ (match disposition with Some d -> [ ("Content-Disposition", d) ] | None -> []);
    body = b64 body
  }

let text_entity t = leaf ~content_type:"text/plain; charset=UTF-8" t
let html_entity h = leaf ~content_type:"text/html; charset=UTF-8" h

let attach_entity (a : attachment) =
  leaf
    ~content_type:(Printf.sprintf "%s; name=\"%s\"" a.content_type a.filename)
    ~disposition:(Printf.sprintf "attachment; filename=\"%s\"" a.filename)
    a.content

let bnd = Atomic.make 0

let multipart subtype (parts : entity list) =
  let b = Printf.sprintf "=_fennec_%s_%d_%.0f=" subtype (Atomic.fetch_and_add bnd 1) (Unix.gettimeofday ()) in
  let body = String.concat "" (List.map (fun p -> "--" ^ b ^ crlf ^ render p ^ crlf) parts) ^ "--" ^ b ^ "--" ^ crlf in
  { headers = [ ("Content-Type", Printf.sprintf "multipart/%s; boundary=\"%s\"" subtype b) ]; body }

(* the body entity for the text/html content, before any attachment wrapping *)
let body_core text html =
  match (text, html) with
  | Some t, Some h -> multipart "alternative" [ text_entity t; html_entity h ]
  | None, Some h -> html_entity h
  | Some t, None -> text_entity t
  | None, None -> text_entity "" (* serialize's callers guarantee a body; this is a defensive default *)

let serialize ~from ~to_ ~cc ?reply_to ~subject ?text ?html ?(headers = []) ?(attachments = []) () =
  let core = body_core text html in
  let body_entity = match attachments with [] -> core | atts -> multipart "mixed" (core :: List.map attach_entity atts) in
  let top =
    [ ("Date", date_header (Unix.gettimeofday ())); ("From", A.to_header from); ("To", A.header_list to_) ]
    @ (match cc with [] -> [] | _ -> [ ("Cc", A.header_list cc) ])
    @ (match reply_to with Some r -> [ ("Reply-To", A.to_header r) ] | None -> [])
    @ [ ("Subject", encode_subject subject); ("Message-ID", message_id (domain_of from)); ("MIME-Version", "1.0") ]
    @ headers
  in
  (* the body entity's own Content-Type / Transfer-Encoding live at the top level of the message *)
  render { headers = top @ body_entity.headers; body = body_entity.body }

(* ---- inline tests ---- *)

let contains hay sub =
  let hl = String.length hay and sl = String.length sub in
  let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
  sl = 0 || go 0

let ada = A.v ~name:"Ada" "ada@example.com"
let bob = A.v "bob@example.com"

let%test "text+html becomes multipart/alternative with both parts base64-encoded" =
  let m = serialize ~from:ada ~to_:[ bob ] ~cc:[] ~subject:"Hi" ~text:"plain" ~html:"<b>rich</b>" () in
  contains m "Content-Type: multipart/alternative;"
  && contains m "Content-Type: text/plain; charset=UTF-8"
  && contains m "Content-Type: text/html; charset=UTF-8"
  && contains m (Base64.encode_string "plain")
  && contains m (Base64.encode_string "<b>rich</b>")
  && contains m "\r\n" (* CRLF line endings *)

let%test "html-only is a single text/html part (no multipart)" =
  let m = serialize ~from:ada ~to_:[ bob ] ~cc:[] ~subject:"Hi" ~html:"<p>x</p>" () in
  contains m "Content-Type: text/html; charset=UTF-8" && not (contains m "multipart/")

let%test "attachments wrap the body in multipart/mixed with a filename disposition" =
  let m =
    serialize ~from:ada ~to_:[ bob ] ~cc:[] ~subject:"Hi" ~text:"hi"
      ~attachments:[ { filename = "r.pdf"; content_type = "application/pdf"; content = "%PDF" } ]
      ()
  in
  contains m "Content-Type: multipart/mixed;"
  && contains m "Content-Disposition: attachment; filename=\"r.pdf\""
  && contains m (Base64.encode_string "%PDF")

let%test "required headers are present; a non-ASCII subject is RFC-2047 encoded; Cc only when given" =
  let m = serialize ~from:ada ~to_:[ bob ] ~cc:[ ada ] ~subject:"Café" ~text:"x" () in
  contains m "From: Ada <ada@example.com>"
  && contains m "To: bob@example.com"
  && contains m "Cc: Ada <ada@example.com>"
  && contains m "MIME-Version: 1.0"
  && contains m "Message-ID: <"
  && contains m ("Subject: =?UTF-8?B?" ^ Base64.encode_string "Café" ^ "?=")
  && not (contains (serialize ~from:ada ~to_:[ bob ] ~cc:[] ~subject:"s" ~text:"x" ()) "Cc:")
