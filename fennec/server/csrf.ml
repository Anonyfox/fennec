(* CSRF protection (Plug.CSRFProtection equivalent). A per-session secret token guards
   state-changing requests: a form embeds {!token} (a fresh MASKED encoding of the secret,
   so the value differs on every render — defeating BREACH-style compression oracles), and
   {!plug} rejects an unsafe request whose submitted token doesn't unmask to the session
   secret. Verification is constant-time.

   Requires {!Session.plug} earlier in the pipeline (the secret lives in the session). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

let token_len = 18 (* raw secret bytes *)
let session_key = "_csrf_token"

let b64e (s : string) : string = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d (s : string) : string option =
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

(* cryptographically-strong random bytes from the OS CSPRNG *)
let secure_random (n : int) : string =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)

(* byte-wise XOR of two equal-length strings *)
let xor (a : string) (b : string) : string =
  String.mapi (fun i ch -> Char.chr (Char.code ch lxor Char.code b.[i])) a

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i ch -> acc := !acc lor (Char.code ch lxor Char.code b.[i])) a;
  !acc = 0

(* the session's raw secret, creating + storing one if absent (for [token]) *)
let secret (c : Conn.t) : string =
  match Option.bind (Session.get c session_key) b64d with
  | Some raw when String.length raw = token_len -> raw
  | _ ->
    let raw = secure_random token_len in
    ignore (Session.set c session_key (b64e raw));
    raw

(* the session's raw secret WITHOUT creating one (for [verify]) *)
let secret_opt (c : Conn.t) : string option =
  match Option.bind (Session.get c session_key) b64d with
  | Some raw when String.length raw = token_len -> Some raw
  | _ -> None

(* a masked, embeddable token — different on every call, all valid for the same secret *)
let token (c : Conn.t) : string =
  let raw = secret c in
  let mask = secure_random token_len in
  b64e (mask ^ xor mask raw)

(* does [submitted] unmask to the session secret? (constant-time, no session mutation) *)
let verify (c : Conn.t) (submitted : string) : bool =
  match (b64d submitted, secret_opt c) with
  | Some s, Some raw when String.length s = 2 * token_len ->
    let mask = String.sub s 0 token_len and masked = String.sub s token_len token_len in
    constant_eq (xor mask masked) raw
  | _ -> false

(* The CSRF plug: verify the token on unsafe methods, decline otherwise. The token comes
   from the [header] (X-CSRF-Token) or a [field] in the form body. *)
let plug ?(field = "_csrf_token") ?(header = "x-csrf-token")
    ?(safe = [ "GET"; "HEAD"; "OPTIONS" ]) () : Paw.t =
 fun c ->
  if List.mem (H.string_of_meth (Conn.meth c)) safe then c
  else
    let submitted =
      match Conn.req_header c header with Some v -> Some v | None -> Conn.body_param c field
    in
    match submitted with
    | Some tok when verify c tok -> c
    | _ -> Conn.text ~status:403 c "CSRF token invalid or missing"
