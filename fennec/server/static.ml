(* Static file serving for the public/ tree. One [source] abstracts the two
   modes:
     - Dir path: read from disk (dev — edits are live)
     - Embedded (name, map): path -> bytes baked into the binary at build time (prod —
       single self-contained executable; name identifies the bundle in the cache)

   Serving is airtight: correct Content-Type (by extension), strong ETag (content
   hash) + conditional 304, Last-Modified, Cache-Control, Range requests (206 /
   416), HEAD. Compression is applied by [Responder.finalize] on the way out for
   the whole-body case; Range responses are sent uncompressed (correct: ranges
   are over the identity representation). *)

module H = Fennec_core.Http
module Sem = Fennec_core.Http_semantics
module Date = Fennec_core.Http_date
module Mime = Fennec_core.Mime

type entry = { bytes : string; etag : string; mtime : float }

(* [Dir path]: read from disk (dev). [Embedded (name, lookup)]: a path -> bytes function
   baked into the binary (prod); the ETag is the content hash and Last-Modified is a fixed
   build epoch (assets are immutable for the binary's lifetime). [name] identifies the bundle
   so two distinct embedded sources in one process can't collide in the shared cache. *)
type source = Dir of string | Embedded of string * (string -> string option)

(* a fixed mtime for embedded assets: they don't change for the binary's life.
   0.0 (epoch) is fine — clients validate via the strong ETag, not the date. *)
let embedded_mtime = 0.0

(* normalize a URL path to a safe relative lookup key:
   - strip the leading '/'
   - reject any segment that is "" (//), "." or ".." (path traversal), or that
     contains a NUL or control byte (defends against open() truncation tricks)
   - map "/" (or a trailing slash) to ".../index.html"
   Returns None if unsafe. Note: this guards the KEY; [dir_lookup] additionally
   verifies the RESOLVED path stays under root (symlink escape). *)
let has_ctrl s = String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7f) s

let safe_key (url_path : string) : string option =
  let p =
    if String.length url_path > 0 && url_path.[0] = '/' then
      String.sub url_path 1 (String.length url_path - 1)
    else url_path
  in
  let p = if p = "" then "index.html" else p in
  let segs = String.split_on_char '/' p in
  let bad = List.exists (fun s -> s = "" || s = "." || s = ".." || has_ctrl s) segs in
  if bad then None
  else
    (* a trailing-slash dir request -> index.html within it *)
    let p = if String.length p > 0 && p.[String.length p - 1] = '/' then p ^ "index.html" else p in
    Some p

(* realpath(root) is constant for the process; resolve it once per root rather than on
   every request. Guarded — the server may run on several domains. *)
let roots_mutex = Mutex.create ()
let roots : (string, string) Hashtbl.t = Hashtbl.create 4

let root_realpath (root : string) : string =
  Mutex.lock roots_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock roots_mutex) (fun () ->
      match Hashtbl.find_opt roots root with
      | Some rp -> rp
      | None ->
        let rp = try Unix.realpath root with _ -> root in
        Hashtbl.replace roots root rp;
        rp)

(* canonicalize and confirm [path] resolves to a real file UNDER [root_rp] — blocks a
   symlink inside the public tree pointing outside it. Returns the realpath, or None if it
   escapes / doesn't exist / is a directory. *)
let resolve_under_root ~root_rp ~path : string option =
  match Unix.realpath path with
  | rp ->
    let prefix = root_rp ^ "/" in
    let under =
      rp = root_rp
      || (String.length rp > String.length prefix && String.sub rp 0 (String.length prefix) = prefix)
    in
    if under && (try not (Sys.is_directory rp) with _ -> false) then Some rp else None
  | exception _ -> None

(* The asset cache holds each served file's bytes + precomputed strong ETag, so the
   whole-file read (Dir) and the full-body hash (both modes) happen ONCE, not per request.
   It is shared across worker domains, so every access is Mutex-guarded (a bare Hashtbl is
   not safe under OCaml 5 multicore). A [revalidate] of [Some (mtime,size)] (Dir) re-reads
   when the file changes on disk; [None] (Embedded) is immutable for the binary's life. The
   cache is LRU-bounded by total body bytes so a large tree can't grow it without limit. *)
type cached = { entry : entry; len : int; mutable atime : int; revalidate : (float * int) option }

let cache_mutex = Mutex.create ()
let cache : (string, cached) Hashtbl.t = Hashtbl.create 64
let cache_clock = ref 0
let cache_bytes = ref 0

(* total cached body bytes to retain before evicting least-recently-used entries *)
let cache_budget = ref (64 * 1024 * 1024)

let with_cache f = Mutex.lock cache_mutex; Fun.protect ~finally:(fun () -> Mutex.unlock cache_mutex) f

(* drop one entry (lock held) *)
let cache_remove k =
  match Hashtbl.find_opt cache k with
  | Some c -> cache_bytes := !cache_bytes - c.len; Hashtbl.remove cache k
  | None -> ()

(* evict least-recently-used entries until within budget (lock held) *)
let evict_to_budget () =
  while !cache_bytes > !cache_budget && Hashtbl.length cache > 0 do
    let oldest =
      Hashtbl.fold
        (fun k c acc -> match acc with Some (_, a) when a <= c.atime -> acc | _ -> Some (k, c.atime))
        cache None
    in
    match oldest with Some (k, _) -> cache_remove k | None -> cache_bytes := 0
  done

let cache_find k : cached option =
  with_cache (fun () ->
      match Hashtbl.find_opt cache k with
      | Some c -> incr cache_clock; c.atime <- !cache_clock; Some c
      | None -> None)

let cache_put k (entry : entry) (revalidate : (float * int) option) =
  with_cache (fun () ->
      cache_remove k;
      incr cache_clock;
      let len = String.length entry.bytes in
      Hashtbl.replace cache k { entry; len; atime = !cache_clock; revalidate };
      cache_bytes := !cache_bytes + len;
      evict_to_budget ())

(* read + hash a file from disk on a miss; the strong ETag is the content hash *)
let load_file (real : string) (mtime : float) : entry option =
  try
    let bytes = In_channel.with_open_bin real In_channel.input_all in
    Some { bytes; etag = Sem.make_etag (Digest.to_hex (Digest.string bytes)); mtime }
  with _ -> None

let dir_lookup (root : string) (key : string) : entry option =
  let path = Filename.concat root key in
  match resolve_under_root ~root_rp:(root_realpath root) ~path with
  | None -> None
  | Some real -> (
    match Unix.stat real with
    | exception _ -> cache_remove ("d:" ^ real); None
    | st -> (
      let mtime = st.Unix.st_mtime and size = st.Unix.st_size in
      let k = "d:" ^ real in
      match cache_find k with
      | Some c when c.revalidate = Some (mtime, size) -> Some c.entry
      | _ -> (
        match load_file real mtime with
        | Some e -> cache_put k e (Some (mtime, size)); Some e
        | None -> None)))

(* Embedded assets are immutable, so hash once and keep — keyed by bundle name + path so two
   distinct embedded sources can't serve each other's bytes. *)
let embedded_lookup (name : string) (f : string -> string option) (key : string) : entry option =
  let k = "e:" ^ name ^ "\x00" ^ key in
  match cache_find k with
  | Some c -> Some c.entry
  | None -> (
    match f key with
    | None -> None
    | Some bytes ->
      let e = { bytes; etag = Sem.make_etag (Digest.to_hex (Digest.string bytes)); mtime = embedded_mtime } in
      cache_put k e None;
      Some e)

let lookup (src : source) (key : string) : entry option =
  match src with Dir root -> dir_lookup root key | Embedded (name, f) -> embedded_lookup name f key

(* Build a response for a static asset. Returns None when there is no such asset
   (caller falls through to the next route / 404). [now] is epoch seconds. *)
let respond ?(cache_control = "public, max-age=3600") (src : source) (req : H.request) :
    H.response option =
  match safe_key req.H.path with
  | None -> Some (H.text ~status:403 "Forbidden")
  | Some key -> (
    match lookup src key with
    | None -> None
    | Some e ->
      let ct = Mime.of_path key in
      let base_headers =
        [ ("Content-Type", ct);
          ("ETag", e.etag);
          ("Last-Modified", Date.format e.mtime);
          ("Cache-Control", cache_control);
          ("Accept-Ranges", "bytes") ]
      in
      let len = String.length e.bytes in
      (* conditional: If-None-Match / If-Modified-Since -> 304 (handled centrally
         too, but we short-circuit here to skip Range work) *)
      let not_modified =
        Sem.if_none_match_satisfied ~etag:e.etag req.H.headers
        || ((Sem.header req.H.headers "if-none-match" = None)
           && Sem.if_modified_since_satisfied ~mtime:e.mtime req.H.headers)
      in
      if not_modified && (req.H.meth = H.GET || req.H.meth = H.HEAD) then
        Some { H.status = 304; headers = base_headers; body = "" }
      else
        (* Range (single range only); ranges are over identity bytes *)
        match Sem.parse_range ~len req.H.headers with
        | `Unsatisfiable ->
          Some
            {
              H.status = 416;
              headers =
                ("Content-Range", Printf.sprintf "bytes */%d" len) :: base_headers;
              body = "";
            }
        | `Range { first; last } ->
          (* clamp defensively so a bad range can never raise (which would drop
             the connection instead of returning a clean response) *)
          let first = max 0 (min first (len - 1)) in
          let last = max first (min last (len - 1)) in
          let slice = String.sub e.bytes first (last - first + 1) in
          let headers =
            ("Content-Range", Printf.sprintf "bytes %d-%d/%d" first last len)
            :: ("Content-Length", string_of_int (String.length slice))
            :: ("Content-Encoding", "identity") (* tell Responder not to gzip *)
            :: base_headers
          in
          Some
            {
              H.status = 206;
              headers;
              body = (if req.H.meth = H.HEAD then "" else slice);
            }
        | `None ->
          Some { H.status = 200; headers = base_headers; body = e.bytes })

(* mount as a fallthrough handler: returns Some response when the path matches an
   asset, None otherwise (so the caller can fall through to pages / 404). *)
let handler ?cache_control (src : source) : H.request -> H.response option =
 fun req -> respond ?cache_control src req

(* the static paw: answer when the path matches an asset, else decline (so pages / 404
   follow). This is the form you drop into an endpoint pipeline. *)
let make ?cache_control (src : source) : Fennec_paw.Paw.t =
  Fennec_paw.Paw.fallthrough (handler ?cache_control src)
