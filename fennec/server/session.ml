(* Sessions (Plug.Session / Dream-grade). A small [string -> string] map per request, with
   two stores:

   - the default SIGNED-COOKIE store: the data is serialized into a cookie, HMAC-SHA256
     signed with a server secret so the client can read but not tamper with it (signed, not
     encrypted — don't put secrets in it). Stateless, so it scales horizontally for free.
   - an optional SERVER-SIDE store ([?store]): the cookie holds only a signed session id and
     the data lives in the store ({!memory_store}, or your own Redis/etc.).

   Either way the session has a [lifetime]: an expired session loads empty, and a session
   past half its life is auto-refreshed (its cookie re-set) on the next request. Add {!plug}
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
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)

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

(* read/write the session from any paw downstream of {!plug}. Reserved "_"-prefixed keys are
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

(* The session plug. [secret] signs the cookie; [lifetime] is the session's max age (and
   the cookie Max-Age). With [store], the cookie holds a signed session id and the data
   lives server-side; without it, the cookie holds the signed data. *)
let plug ~(secret : string) ?(cookie = "_fennec_session") ?(path = "/") ?(lifetime = 86400.)
    ?(same_site = Cookie.Lax) ?(http_only = true) ?secure ?store () : Paw.t =
 fun c ->
  let secure = match secure with Some b -> b | None -> Conn.scheme c = "https" in
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
