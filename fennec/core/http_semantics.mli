(** Pure HTTP semantics — content-encoding negotiation, conditional requests, and
    Range parsing (RFC 7230/7231/7232/7233). Stdlib only; the server layer wires
    these to IO.

    {[
      module Sem = Http_semantics

      let etag = Sem.make_etag (Digest.to_hex (Digest.string body)) in
      if Sem.if_none_match_satisfied ~etag req_headers then `Not_modified (* 304 *)
      else
        match Sem.negotiate_encoding ~accept:(Sem.header req_headers "accept-encoding") () with
        | Sem.Gzip -> `Send_gzip
        | _ -> `Send_identity
    ]} *)

(** Case-insensitive header lookup over an assoc list. *)
val header : (string * string) list -> string -> string option

(** A content-coding the server supports. *)
type encoding = Identity | Gzip | Deflate

(** The wire token for an encoding (["gzip"], ["deflate"], ["identity"]). *)
val encoding_token : encoding -> string

(** Pick the best supported encoding given an [Accept-Encoding] value (q-values
    honoured; ["*"] handled; q=0 forbids; ties prefer gzip). Absent/empty header
    or no acceptable coding yields {!Identity}. *)
val negotiate_encoding : ?accept:string option -> unit -> encoding

(** Wrap a content-hash hex string as a strong, quoted ETag. *)
val make_etag : string -> string

(** [If-None-Match] satisfied for [etag] (handles ["*"], lists, and W/ weak
    prefixes for cache validation). *)
val if_none_match_satisfied : etag:string -> (string * string) list -> bool

(** [If-Modified-Since] satisfied: true (=> 304) when [mtime] (epoch seconds) is
    not newer than the client's date. Malformed/absent date => false. *)
val if_modified_since_satisfied : mtime:float -> (string * string) list -> bool

(** A resolved, inclusive byte range. *)
type range = { first : int; last : int }

(** Parse a single-range [Range] header against a body of length [len]:
    - [`None] — no/declined range (absent, non-bytes unit, or multi-range)
    - [`Range r] — a satisfiable range, clamped to the body
    - [`Unsatisfiable] — caller should answer 416 *)
val parse_range :
  len:int -> (string * string) list -> [ `None | `Range of range | `Unsatisfiable ]
