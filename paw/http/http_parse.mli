(** The zero-copy request-HEAD parser — paw's one hand-crafted C hot path (see
    {!file:http_parse_stubs.c}). Scans the request line + headers in place over an Eio buffer (a
    Bigarray) and writes byte offsets into a caller-preallocated int array: zero allocation, no global
    state, fully reentrant (each connection/domain parses its own buffer + output, so it is
    parallel-safe with no synchronization), and crash-safe (every byte read is bounds-checked in C —
    malformed input returns a code, never a fault). Used only by {!Server.read_request}. *)

(** [parse buf off len out maxh] parses the request HEAD in [buf]'s window [[off, off+len)] into [out]
    (which must have capacity {!out_size}, i.e. [7 + 4*maxh]). On success returns the number of bytes
    the head occupies (advance the reader by exactly this); the [out] layout is then:
    {v
      out.(0),out.(1)  method  offset,len
      out.(2),out.(3)  target  offset,len   (raw request-target: path[?query], not yet split)
      out.(4),out.(5)  version offset,len
      out.(6)          header count
      header i:  out.(7+4i+0..3) = name_off, name_len, value_off, value_len  (value OWS-trimmed)
    v}
    All offsets are absolute into [buf]. On failure returns {!incomplete} (head not fully buffered —
    read more and call again), {!malformed} (answer 400), or {!too_many} (more headers than [maxh]). *)
external parse : Bigstringaf.t -> int -> int -> int array -> int -> int = "paw_http_parse" [@@noalloc]

(** Returned by {!parse} when the head is not yet fully buffered. *)
val incomplete : int

(** Returned by {!parse} on a malformed request line / head. *)
val malformed : int

(** Returned by {!parse} when the head has more headers than the output array can hold. *)
val too_many : int

(** The header cap passed as [maxh] to {!parse} (mirrors the prior [max_header_count]). *)
val max_headers : int

(** The required length of the [out] array for {!parse} ([7 + 4 * max_headers]). *)
val out_size : int

(** A fresh, correctly-sized [out] scratch array — allocate one per connection and reuse it. *)
val make_out : unit -> int array
