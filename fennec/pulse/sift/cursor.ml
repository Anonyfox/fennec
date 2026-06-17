(* cursor.ml — a bounds-checked read head over a byte buffer: the bedrock of the zero-copy kernel.

   The new Sift decode path reads STRAIGHT from a buffer (no intermediate Bson.t tree), shape-directed.
   A Cursor advances through the bytes — BSON's wire form is little-endian + length-prefixed — WITHOUT
   allocating: a scanner reads {!pos} before and after a value to delimit its bytes as a (offset, length)
   SPAN into the buffer (a borrowed slice, never copied). Every read is bounds-checked against a [limit]
   (a nested sub-document's end), so a truncated or malformed buffer is rejected, never read past.

   First stone of the K1 bedrock — see the [sift-kernel-concept] design (six-axis kernel, zero-copy tape
   engine). Additive: nothing in the working [Sift.*] facade depends on this yet. *)

type buffer = Bigstringaf.t

(* a read head: the buffer, a mutable position, and a hard [limit] (one past the last readable byte —
   the end of the whole buffer, or of a nested document being scanned) *)
type t = { buf : buffer; mutable pos : int; limit : int }

(* [limit] is clamped to the buffer length — the INVARIANT [limit ≤ Bigstringaf.length buf] is what makes
   the post-[need] [unsafe_get]s provably in-range *)
let make ?(pos = 0) ?limit (buf : buffer) : t =
  let n = Bigstringaf.length buf in
  { buf; pos; limit = (match limit with Some l -> if l < n then l else n | None -> n) }

let pos (c : t) : int = c.pos
let limit (c : t) : int = c.limit
let buffer (c : t) : buffer = c.buf
let remaining (c : t) : int = c.limit - c.pos
let at_end (c : t) : bool = c.pos >= c.limit

(* a sub-cursor over [len] bytes starting at the current position, with its own [limit] — for scanning a
   nested document/array without it being able to read past its own length-prefixed bounds *)
let sub (c : t) ~len : t = { buf = c.buf; pos = c.pos; limit = (let e = c.pos + len in if e < c.limit then e else c.limit) }

(* raised when a read would run past [limit] — a truncated/malformed buffer is rejected, never read OOB *)
exception Truncated of { need : int; have : int }

let need (c : t) (n : int) : unit = if c.pos + n > c.limit then raise (Truncated { need = n; have = c.limit - c.pos })

(* little-endian scalar reads (BSON's wire encoding); each advances [pos]. [need] bounds-checks against
   [limit] FIRST, and [limit ≤ Bigstringaf.length] is an invariant (see {!make}/{!sub}), so the byte is
   provably in range — the read itself is [unsafe_get] (one check, not the two a checked [Bigstringaf.get]
   would do). Safe on hostile input: [need] still rejects a truncated buffer before any read. *)
let uint8 (c : t) : int = need c 1; let b = Char.code (Bigstringaf.unsafe_get c.buf c.pos) in c.pos <- c.pos + 1; b
let int32 (c : t) : int = need c 4; let v = Int32.to_int (Bigstringaf.unsafe_get_int32_le c.buf c.pos) in c.pos <- c.pos + 4; v

(* an UNSIGNED little-endian int32 (BSON timestamps store two u32s) — read jsoo-safely via
   [Int32.unsigned_to_int] (no 0xFFFFFFFF literal, which overflows a 32-bit jsoo int): native gets the
   true unsigned value; a 32-bit runtime, which cannot represent it, falls back to the signed reading *)
let uint32 (c : t) : int =
  need c 4;
  let v = Bigstringaf.unsafe_get_int32_le c.buf c.pos in
  c.pos <- c.pos + 4;
  match Int32.unsigned_to_int v with Some i -> i | None -> Int32.to_int v
let int64 (c : t) : int64 = need c 8; let v = Bigstringaf.unsafe_get_int64_le c.buf c.pos in c.pos <- c.pos + 8; v
let double (c : t) : float = Int64.float_of_bits (int64 c)

let skip (c : t) (n : int) : unit = need c n; c.pos <- c.pos + n

(* jump [pos] to an absolute offset (within bounds) — the zero-copy decoder rewinds to a document's
   start to scan it for the next wanted field (BSON fields may be in any order), reusing ONE cursor
   instead of allocating a fresh one per field *)
let seek (c : t) (p : int) : unit = if p < 0 || p > c.limit then raise (Truncated { need = p - c.limit; have = 0 }); c.pos <- p

(* advance past [len] bytes, returning their start offset — the caller pairs (off, len) as a span *)
let take (c : t) ~len : int = need c len; let off = c.pos in c.pos <- c.pos + len; off

(* a NUL-terminated C string (BSON keys): advance past it (and its NUL); return the byte LENGTH before
   the NUL — NOT a (offset,length) tuple, since the caller already knows the offset ([Cursor.pos] before
   the call), so the scan stays allocation-free in the per-field hot loop. Rejects an unterminated key. *)
let key_len (c : t) : int =
  let start = c.pos in
  let rec scan () =
    if c.pos >= c.limit then raise (Truncated { need = 1; have = 0 })
    else if Bigstringaf.unsafe_get c.buf c.pos = '\000' then (let len = c.pos - start in c.pos <- c.pos + 1; len)
    else (c.pos <- c.pos + 1; scan ())
  in
  scan ()

(* copy a span out as an owned OCaml string — the T2 / owned-materialization path (T1 keeps it borrowed) *)
let substring (c : t) ~off ~len : string = Bigstringaf.substring c.buf ~off ~len

(* test the byte at [off] equals char [ch] without advancing — for span/byte comparisons during validation *)
let byte_at (c : t) (off : int) : char = Bigstringaf.get c.buf off

(* a little-endian int32 at an ABSOLUTE offset, WITHOUT moving [pos] — the scanner computes a value's
   byte length (a nested doc/string is length-prefixed) to skip it, before deciding to read it. Bounds-
   checked against [limit] so a garbage length can't peek out of the buffer. *)
let peek_int32 (c : t) (at : int) : int =
  if at < c.pos || at + 4 > c.limit then raise (Truncated { need = 4; have = c.limit - at });
  Int32.to_int (Bigstringaf.unsafe_get_int32_le c.buf at)

(* compare a buffer span [off, len) to an OCaml string WITHOUT copying it out — the zero-copy key match
   (a record field's name vs a scanned doc key). Returns false on any byte mismatch or length mismatch. *)
let span_eq_string (c : t) ~off ~len (s : string) : bool =
  len = String.length s
  &&
  let rec go i = i >= len || (Bigstringaf.unsafe_get c.buf (off + i) = String.unsafe_get s i && go (i + 1)) in
  go 0
