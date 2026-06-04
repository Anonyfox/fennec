(* The airtight HTTP bits, kept PURE (Stdlib only) so every rule is unit-testable
   without a socket: content-encoding negotiation (Accept-Encoding + q-values),
   conditional requests (ETag / If-None-Match, Last-Modified / If-Modified-Since),
   and Range parsing (RFC 7233). The server layer wires these to actual IO. *)

(* ---- small header helpers ---- *)

(* case-insensitive header lookup (allocation-free; see {!Headers}) *)
let header (headers : (string * string) list) (name : string) : string option =
  Headers.get headers name

let trim = String.trim
let lower = String.lowercase_ascii

let split_commas (s : string) : string list =
  String.split_on_char ',' s |> List.map trim |> List.filter (fun x -> x <> "")

(* ---- content-encoding negotiation ---- *)

type encoding = Identity | Gzip | Deflate

let encoding_token = function Identity -> "identity" | Gzip -> "gzip" | Deflate -> "deflate"

(* Parse one "token;q=0.5" element into (token, q). Missing q defaults to 1.0;
   a malformed q is treated as 1.0. q is clamped to [0,1]. *)
let parse_coding (elt : string) : string * float =
  match String.split_on_char ';' elt with
  | [] -> ("", 1.0)
  | tok :: params ->
    let tok = lower (trim tok) in
    let q =
      List.fold_left
        (fun acc p ->
          match String.split_on_char '=' p with
          | [ k; v ] when lower (trim k) = "q" -> (
            match float_of_string_opt (trim v) with Some f -> f | None -> acc)
          | _ -> acc)
        1.0 params
    in
    (tok, Float.max 0.0 (Float.min 1.0 q))

(* Given the request's Accept-Encoding, pick the best SUPPORTED encoding (we
   support gzip + deflate). RFC 7231 semantics:
   - absent header => identity is acceptable (return Identity)
   - q=0 forbids an encoding
   - "*" sets the default for codings not explicitly listed
   - identity is acceptable unless explicitly forbidden (identity;q=0 or *;q=0)
   Preference order among acceptable codings we support: gzip > deflate. *)
let negotiate_encoding ?(accept = None) () : encoding =
  match accept with
  | None | Some "" -> Identity
  | Some s ->
    let codings = List.map parse_coding (split_commas s) in
    let q_of tok =
      (* explicit token wins; else "*"; else default 1.0 for identity, 0 unknown *)
      match List.assoc_opt tok codings with
      | Some q -> Some q
      | None -> (
        match List.assoc_opt "*" codings with
        | Some q -> Some q
        | None -> if tok = "identity" then Some 1.0 else None)
    in
    let gzip_q = Option.value ~default:0.0 (q_of "gzip") in
    let deflate_q = Option.value ~default:0.0 (q_of "deflate") in
    if gzip_q > 0.0 && gzip_q >= deflate_q then Gzip
    else if deflate_q > 0.0 then Deflate
    else Identity

(* ---- ETag / conditional requests ---- *)

(* A strong ETag is fine for our static content (we hash the bytes). Quote it. *)
let make_etag (content_hash_hex : string) : string = Printf.sprintf "\"%s\"" content_hash_hex

(* If-None-Match: a comma list of ETags or "*". Match is by entity-tag equality
   (we treat W/ weak prefixes as matching their strong form for GET/HEAD, which
   is what RFC 7232 allows for cache validation). *)
let if_none_match_satisfied ~etag (headers : (string * string) list) : bool =
  match header headers "if-none-match" with
  | None -> false
  | Some v ->
    let v = trim v in
    if v = "*" then true
    else
      let strip t =
        let t = trim t in
        let t = if String.length t >= 2 && String.sub t 0 2 = "W/" then String.sub t 2 (String.length t - 2) else t in
        trim t
      in
      List.exists (fun t -> strip t = strip etag) (split_commas v)

(* If-Modified-Since: true (=> 304) when the resource's mtime is NOT newer than
   the date the client holds. We compare as HTTP-date strings only when equal;
   for correctness we compare parsed epoch seconds. *)
let if_modified_since_satisfied ~mtime (headers : (string * string) list) : bool =
  match header headers "if-modified-since" with
  | None -> false
  | Some v -> (
    match Http_date.parse v with Some t -> mtime <= t | None -> false)

(* ---- Range (RFC 7233), single range only ---- *)

type range = { first : int; last : int } (* inclusive, resolved against length *)

(* Parse "bytes=START-END" / "bytes=START-" / "bytes=-SUFFIX" against [len].
   Returns None for: non-bytes units, multiple ranges (we serve the whole body),
   or unsatisfiable ranges (caller should 416). Returns Some range otherwise. *)
let parse_range ~len (headers : (string * string) list) : [ `None | `Range of range | `Unsatisfiable ] =
  match header headers "range" with
  | None -> `None
  | Some v -> (
    let v = trim v in
    let prefix = "bytes=" in
    let plen = String.length prefix in
    if String.length v <= plen || lower (String.sub v 0 plen) <> prefix then `None
    else
      let spec = String.sub v (String.length prefix) (String.length v - String.length prefix) in
      (* multiple ranges (comma) -> we decline, serve full body *)
      if String.contains spec ',' then `None
      else
        match String.split_on_char '-' spec with
        | [ a; b ] -> (
          let a = trim a and b = trim b in
          match (a, b) with
          | "", "" -> `None
          | "", suffix -> (
            (* last N bytes *)
            match int_of_string_opt suffix with
            | Some n when n > 0 ->
              let first = Stdlib.max 0 (len - n) in
              `Range { first; last = len - 1 }
            | _ -> `Unsatisfiable)
          | start, "" -> (
            match int_of_string_opt start with
            | Some s when s < len -> `Range { first = s; last = len - 1 }
            | Some _ -> `Unsatisfiable
            | None -> `None)
          | start, stop -> (
            match (int_of_string_opt start, int_of_string_opt stop) with
            | Some s, Some e when s <= e && s < len ->
              `Range { first = s; last = Stdlib.min e (len - 1) }
            | Some s, Some _ when s >= len -> `Unsatisfiable
            | _ -> `None))
        | _ -> `None)
