(* Sessions (Dream-grade). A small [string -> string] map per request, with two stores:

   - the default SIGNED-COOKIE store: the data is serialized into a cookie, HMAC-SHA256
     signed with a server secret so the client can read but not tamper with it (signed, not
     encrypted — don't put secrets in it). Stateless, so it scales horizontally for free.
   - an optional SERVER-SIDE store ([?store]): the cookie holds only a signed session id and
     the data lives in the store ({!memory_store}, or your own Redis/etc.).

   Either way the session has a [lifetime]: an expired session loads empty, and a session
   past half its life is auto-refreshed (its cookie re-set) on the next request. Add {!make}
   early in a pipeline, then read/write with {!get}/{!set} downstream. Constant-time verify. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Assigns = Fennec_paw.Assigns
module H = Fennec_core.Http
module Cookie = Fennec_core.Cookie

(* ---- crypto + (de)serialization (pure) ---- *)

let b64e (s : string) : string = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d (s : string) : string option =
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None
let hmac ~(secret : string) (msg : string) : string =
  Digestif.SHA256.(to_raw_string (hmac_string ~key:secret msg))
let now () = Unix.gettimeofday ()
let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg ->
    (* fail CLOSED: never weaken a session secret with a non-CSPRNG fallback *)
    failwith ("fennec: secure randomness unavailable (/dev/urandom): " ^ msg)

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

(* "<b64 payload>.<b64 hmac(payload)>" *)
let sign ~(secret : string) (payload : string) : string =
  let p = b64e payload in
  p ^ "." ^ b64e (hmac ~secret p)

let verify ~(secret : string) (token : string) : string option =
  match String.index_opt token '.' with
  | None -> None
  | Some i ->
    let p = String.sub token 0 i in
    let sig_ = String.sub token (i + 1) (String.length token - i - 1) in
    if constant_eq sig_ (b64e (hmac ~secret p)) then b64d p else None

(* the session map <-> payload. An "_exp=<epoch>" entry carries the expiry; user keys live
   alongside it. *)
let encode ~(exp : float) (data : (string * string) list) : string =
  String.concat "&"
    (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v)
       (("_exp", Printf.sprintf "%.0f" exp) :: data))

(* decode -> (user data without _exp, expiry if present) *)
let decode (payload : string) : (string * string) list * float option =
  let pairs = H.parse_query payload in
  let exp = Option.bind (List.assoc_opt "_exp" pairs) float_of_string_opt in
  (List.filter (fun (k, _) -> k <> "_exp") pairs, exp)

(* ---- a server-side store interface + an in-memory implementation ---- *)

type store = {
  load : string -> (string * string) list option; (* session id -> data *)
  save : string -> (string * string) list -> unit;
  delete : string -> unit;
}

(* a process-local store backed by a Mutex-guarded Hashtbl (domain-safe), with a TTL so
   abandoned sessions are evicted. For a single machine; swap in Redis/etc. via {!store}. *)
let memory_store ?(ttl = 86400.) () : store =
  let tbl : (string, float * (string * string) list) Hashtbl.t = Hashtbl.create 256 in
  let m = Mutex.create () in
  let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f in
  let load id =
    locked (fun () ->
        match Hashtbl.find_opt tbl id with
        | Some (exp, data) when now () < exp -> Some data
        | Some _ -> Hashtbl.remove tbl id; None
        | None -> None)
  in
  let save id data = locked (fun () -> Hashtbl.replace tbl id (now () +. ttl, data)) in
  let delete id = locked (fun () -> Hashtbl.remove tbl id) in
  { load; save; delete }

(* ---- the per-request session state, carried in a typed assign ---- *)

type t = { mutable data : (string * string) list; loaded : (string * string) list; loaded_exp : float option; id : string option }

let key : t Assigns.key = Assigns.key "fennec.session"
let current (c : Conn.t) : t option = Conn.get c key

(* whether {!make} ran upstream on this conn (so the session assign is present) — lets a
   dependent paw (e.g. CSRF) fail loudly on a misordered pipeline instead of silently. *)
let active (c : Conn.t) : bool = current c <> None

(* read/write the session from any paw downstream of {!make}. Reserved "_"-prefixed keys are
   hidden from [get_all]. *)
let get (c : Conn.t) (k : string) : string option =
  match current c with Some s -> List.assoc_opt k s.data | None -> None

let get_all (c : Conn.t) : (string * string) list =
  match current c with
  | Some s -> List.filter (fun (k, _) -> String.length k = 0 || k.[0] <> '_') s.data
  | None -> []

let set (c : Conn.t) (k : string) (v : string) : Conn.t =
  (match current c with Some s -> s.data <- (k, v) :: List.remove_assoc k s.data | None -> ());
  c

let delete (c : Conn.t) (k : string) : Conn.t =
  (match current c with Some s -> s.data <- List.remove_assoc k s.data | None -> ());
  c

let clear (c : Conn.t) : Conn.t =
  (match current c with Some s -> s.data <- [] | None -> ());
  c

(* the request scheme as seen by the client — honouring an upstream X-Forwarded-Proto from a
   TLS-terminating proxy (the standard prod deploy, where [Conn.scheme] is the plain inner
   "http"). The header may list several protos ("https, http"); the first is the client-side
   one. Without this, the session cookie's [Secure] default would be wrong behind a proxy. *)
let forwarded_scheme (c : Conn.t) : string =
  match Conn.req_header c "x-forwarded-proto" with
  | Some p -> (
    match String.split_on_char ',' p with
    | first :: _ when String.trim first <> "" -> String.lowercase_ascii (String.trim first)
    | _ -> Conn.scheme c)
  | None -> Conn.scheme c

(* The session paw. [secret] signs the cookie; [lifetime] is the session's max age (and
   the cookie Max-Age). With [store], the cookie holds a signed session id and the data
   lives server-side; without it, the cookie holds the signed data. *)
let make ~(secret : string) ?(cookie = "_fennec_session") ?(path = "/") ?(lifetime = 86400.)
    ?(same_site = Cookie.Lax) ?(http_only = true) ?secure ?store () : Paw.t =
  (* fail fast on a missing/weak secret — an empty or tiny secret yields real-looking but
     trivially forgeable cookies, with no other signal that anything is wrong *)
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Paw.Session.make: ~secret must be at least 16 bytes (got %d) — use a long random string"
         (String.length secret));
  fun c ->
  let secure = match secure with Some b -> b | None -> forwarded_scheme c = "https" in
  let tok = Conn.cookie c cookie in
  let loaded, loaded_exp, id =
    match store with
    | None -> (
      (* signed-cookie: the cookie carries the data *)
      match Option.bind tok (verify ~secret) with
      | Some payload ->
        let data, exp = decode payload in
        let expired = match exp with Some e -> now () > e | None -> false in
        ((if expired then [] else data), exp, None)
      | None -> ([], None, None))
    | Some st -> (
      (* server-side: the cookie carries a signed session id *)
      match Option.bind tok (verify ~secret) with
      | Some sid -> ((match st.load sid with Some d -> d | None -> []), None, Some sid)
      | None -> ([], None, None))
  in
  let sess = { data = loaded; loaded; loaded_exp; id } in
  let c = Conn.assign c key sess in
  Conn.before_send c (fun r ->
      let changed = List.sort compare sess.data <> List.sort compare sess.loaded in
      let half_expired = match sess.loaded_exp with Some e -> now () > e -. (lifetime /. 2.) | None -> false in
      if (not changed) && not half_expired && (store = None || sess.id <> None) then r
      else
        let value =
          match store with
          | None -> sign ~secret (encode ~exp:(now () +. lifetime) sess.data)
          | Some st ->
            let sid = match sess.id with Some s -> s | None -> b64e (secure_random 18) in
            st.save sid sess.data;
            sign ~secret sid
        in
        let sc =
          Cookie.to_set_cookie ~name:cookie ~value ~path ~max_age:(int_of_float lifetime) ~secure
            ~http_only ~same_site ()
        in
        { r with H.headers = ("set-cookie", sc) :: r.H.headers })

(* ──── sign/verify tests ──── *)

let secret_ = "a-stable-server-secret"

let%test "verify round-trips" =
  verify ~secret:secret_ (sign ~secret:secret_ "hello world") = Some "hello world"
let%test "wrong secret rejected" =
  verify ~secret:"other" (sign ~secret:secret_ "hello world") = None
let%test "tampered token rejected" =
  verify ~secret:secret_ (sign ~secret:secret_ "hello world" ^ "x") = None
let%test "no signature -> rejected" =
  verify ~secret:secret_ "nodot" = None
let%test "empty -> rejected" =
  verify ~secret:secret_ "" = None

(* ──── session paw round-trip tests ──── *)

let req_ ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ()
let finalize_ c = Conn.apply_before_send c (Option.get (Conn.resp c))
let cookie_kv_ set_cookie =
  match String.index_opt set_cookie ';' with Some i -> String.sub set_cookie 0 i | None -> set_cookie
let set_cookie_ c =
  match Fennec_core.Headers.get_all (finalize_ c).H.headers "set-cookie" with s :: _ -> s | [] -> ""

let%test "read within the same request" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  get c1 "user" = Some "ada"

let%test_unit "a session cookie is set after a write" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  Fennec_hunt_unit.check "cookie is set" (set1 <> "");
  Fennec_hunt_unit.check "cookie is HttpOnly" (cookie_kv_ set1 <> set1)

let%test "session restored on the next request" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  let kv = cookie_kv_ set1 in
  let c2 = sp (Conn.make (req_ ~headers:[ ("Cookie", kv) ] "/")) in
  get c2 "user" = Some "ada"

let%test "unchanged session emits no Set-Cookie" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  let kv = cookie_kv_ set1 in
  let c3 = sp (Conn.make (req_ ~headers:[ ("Cookie", kv) ] "/")) in
  let _ = get c3 "user" in
  let c3 = Conn.text c3 "x" in
  let r3 = finalize_ c3 in
  Fennec_core.Headers.get_all r3.H.headers "set-cookie" = []

let%test "tampered cookie yields empty session" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  let kv = cookie_kv_ set1 in
  let c4 = sp (Conn.make (req_ ~headers:[ ("Cookie", kv ^ "x") ] "/")) in
  get c4 "user" = None

let%test "clear empties the session" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  let kv = cookie_kv_ set1 in
  let c5 = sp (Conn.make (req_ ~headers:[ ("Cookie", kv) ] "/")) in
  let c5 = clear c5 in
  get c5 "user" = None

(* ──── session expiry + refresh tests ──── *)

let%test "unexpired session loads its data" =
  let cookie payload = "_fennec_session=" ^ sign ~secret:secret_ payload in
  let c = make ~secret:secret_ () (Conn.make (req_ ~headers:[ ((fun kv -> ("Cookie", kv)) (cookie "_exp=9999999999&user=ada")) ] "/")) in
  get c "user" = Some "ada"

let%test "expired session loads empty" =
  let cookie payload = "_fennec_session=" ^ sign ~secret:secret_ payload in
  let c = make ~secret:secret_ () (Conn.make (req_ ~headers:[ ("Cookie", cookie "_exp=1&user=ada") ] "/")) in
  get c "user" = None

let%test "_exp hidden from get_all" =
  let cookie payload = "_fennec_session=" ^ sign ~secret:secret_ payload in
  let c = make ~secret:secret_ () (Conn.make (req_ ~headers:[ ("Cookie", cookie "_exp=9999999999&user=ada") ] "/")) in
  get_all c = [ ("user", "ada") ]

let%test "half-expired session is refreshed (Set-Cookie emitted)" =
  let cookie payload = "_fennec_session=" ^ sign ~secret:secret_ payload in
  let near = Printf.sprintf "_exp=%.0f&user=ada" (Unix.gettimeofday () +. 10.) in
  let c = make ~secret:secret_ ~lifetime:100. () (Conn.make (req_ ~headers:[ ("Cookie", cookie near) ] "/")) in
  let c = Conn.text c "x" in
  set_cookie_ c <> ""

let%test "fresh unchanged session: no refresh" =
  let cookie payload = "_fennec_session=" ^ sign ~secret:secret_ payload in
  let fresh = Printf.sprintf "_exp=%.0f&user=ada" (Unix.gettimeofday () +. 1000.) in
  let c = make ~secret:secret_ ~lifetime:100. () (Conn.make (req_ ~headers:[ ("Cookie", cookie fresh) ] "/")) in
  let c = Conn.text c "x" in
  set_cookie_ c = ""

(* ──── session server-side store tests ──── *)

let%test_unit "server-side store: write and rehydrate" =
  let store = memory_store () in
  let c1 = make ~secret:secret_ ~store () (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let kv = cookie_kv_ (set_cookie_ c1) in
  let value = match String.index_opt kv '=' with Some i -> String.sub kv (i + 1) (String.length kv - i - 1) | None -> "" in
  let sid = Option.get (verify ~secret:secret_ value) in
  Fennec_hunt_unit.check "data is in the server store" (store.load sid = Some [ ("user", "ada") ]);
  let c2 = make ~secret:secret_ ~store () (Conn.make (req_ ~headers:[ ("Cookie", kv) ] "/")) in
  Fennec_hunt_unit.check "store: session rehydrated by id" (get c2 "user" = Some "ada")

(* ──── session Secure flag (proxy-aware) tests ──── *)

let%test_unit "Secure flag proxy-aware" =
  let cookie_for headers =
    let c = make ~secret:secret_ () (Conn.make (req_ ~headers "/")) in
    set_cookie_ (Conn.text (set c "user" "ada") "ok")
  in
  Fennec_hunt_unit.check "X-Forwarded-Proto=https -> Secure cookie"
    (Fennec_hunt_unit.str_contains (cookie_for [ ("X-Forwarded-Proto", "https") ]) "Secure");
  Fennec_hunt_unit.check "plain http -> cookie not Secure"
    (not (Fennec_hunt_unit.str_contains (cookie_for []) "Secure"))

(* ──── weak-secret guard tests ──── *)

let constructs_ f = match (try Some (f ()) with Invalid_argument _ -> None) with Some _ -> true | None -> false

let%test "empty secret rejected" =
  not (constructs_ (fun () -> make ~secret:"" ()))
let%test "short secret rejected" =
  not (constructs_ (fun () -> make ~secret:"tooshort" ()))
let%test "16+ byte secret accepted" =
  constructs_ (fun () -> make ~secret:(String.make 16 'x') ())

(* ──── promoted sub-checks (standalone) ──── *)

(* these match old test_session assertions that were grouped in test_unit blocks above *)
let%test "cookie is HttpOnly (has attributes after ;)" =
  let sp = make ~secret:secret_ () in
  let c1 = sp (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize_ c1 in
  let set1 = match Fennec_core.Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  cookie_kv_ set1 <> set1  (* has attributes after ';' *)

let%test "store: data is in the server store" =
  let store = memory_store () in
  let c1 = make ~secret:secret_ ~store () (Conn.make (req_ "/")) in
  let c1 = set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let kv = cookie_kv_ (set_cookie_ c1) in
  let value = match String.index_opt kv '=' with Some i -> String.sub kv (i + 1) (String.length kv - i - 1) | None -> "" in
  let sid = Option.get (verify ~secret:secret_ value) in
  store.load sid = Some [ ("user", "ada") ]

let%test "plain http -> cookie not Secure" =
  let c = make ~secret:secret_ () (Conn.make (req_ "/")) in
  let sc = set_cookie_ (Conn.text (set c "user" "ada") "ok") in
  not (Fennec_hunt_unit.str_contains sc "Secure")
