/* Zero-allocation HTTP/1.1 request-HEAD parser for fennec-paw — the one hand-crafted C hot path.
 *
 * Scans the request line + headers IN PLACE over an Eio buffer (a Bigarray) and writes byte OFFSETS
 * into a caller-preallocated OCaml int array. It allocates nothing (OCaml or C), holds no global
 * state, and is fully reentrant — every domain/connection calls it on its own buffer + output array,
 * so it is safe under parallelism with zero synchronization.
 *
 * Crash-safety is the first principle: every single byte read is bounds-checked against the readable
 * window [off, off+len); the parser never reads one byte past what the caller proved is buffered, and
 * the header count is capped by the output-array capacity. Malformed input returns an error code, it
 * never faults. Inspired by picohttpparser/H2O, but scalar + fully bounds-checked (no SIMD over-read).
 *
 * Output layout (OCaml int array `out`, capacity = 7 + 4*max_headers):
 *   out[0]=method_off  out[1]=method_len
 *   out[2]=target_off  out[3]=target_len      (raw request target: path[?query], not yet split)
 *   out[4]=version_off out[5]=version_len
 *   out[6]=n_headers
 *   header i (0-based): out[7+4i+0]=name_off  +1=name_len  +2=value_off  +3=value_len  (value OWS-trimmed)
 * All offsets are ABSOLUTE into the Bigarray base.
 *
 * Return: >= 0  number of bytes the head occupies (advance the reader by exactly this);
 *         -1    incomplete — the head is not fully buffered yet (read more, then call again);
 *         -2    malformed — answer 400 and close;
 *         -3    too many headers for the output array — answer 431/413 and close.
 */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#define PAW_INCOMPLETE (-1)
#define PAW_MALFORMED (-2)
#define PAW_TOO_MANY (-3)

/* a single tab/space (optional whitespace, RFC 9110 OWS) */
static inline int paw_is_ows(unsigned char c) { return c == ' ' || c == '\t'; }

value paw_http_parse(value v_buf, value v_off, value v_len, value v_out, value v_maxh) {
  const unsigned char *b = (const unsigned char *)Caml_ba_data_val(v_buf);
  const intnat base = Long_val(v_off);
  const intnat end = base + Long_val(v_len);
  const intnat maxh = Long_val(v_maxh);
  intnat p = base;

  /* ---- request line: METHOD SP TARGET SP VERSION CRLF ---- */
  intnat method_off = p;
  while (p < end && b[p] != ' ' && b[p] != '\r' && b[p] != '\n') p++;
  if (p >= end) return Val_long(PAW_INCOMPLETE);
  if (b[p] != ' ' || p == method_off) return Val_long(PAW_MALFORMED);
  Field(v_out, 0) = Val_long(method_off);
  Field(v_out, 1) = Val_long(p - method_off);
  p++; /* skip SP */

  intnat target_off = p;
  while (p < end && b[p] != ' ' && b[p] != '\r' && b[p] != '\n') p++;
  if (p >= end) return Val_long(PAW_INCOMPLETE);
  if (b[p] != ' ' || p == target_off) return Val_long(PAW_MALFORMED);
  Field(v_out, 2) = Val_long(target_off);
  Field(v_out, 3) = Val_long(p - target_off);
  p++; /* skip SP */

  intnat version_off = p;
  while (p < end && b[p] != '\r' && b[p] != '\n') p++;
  if (p >= end) return Val_long(PAW_INCOMPLETE);
  intnat version_len = p - version_off;
  if (version_len == 0) return Val_long(PAW_MALFORMED);
  Field(v_out, 4) = Val_long(version_off);
  Field(v_out, 5) = Val_long(version_len);
  /* consume the request-line terminator: CRLF (tolerate a bare LF) */
  if (b[p] == '\r') { p++; if (p >= end) return Val_long(PAW_INCOMPLETE); if (b[p] != '\n') return Val_long(PAW_MALFORMED); }
  p++; /* skip LF */

  /* ---- headers: (NAME ":" OWS VALUE OWS CRLF)* CRLF ---- */
  intnat nh = 0;
  for (;;) {
    if (p >= end) return Val_long(PAW_INCOMPLETE);
    /* blank line terminates the head — record the header count, then return the head length */
    if (b[p] == '\r') { p++; if (p >= end) return Val_long(PAW_INCOMPLETE); if (b[p] != '\n') return Val_long(PAW_MALFORMED); p++; Field(v_out, 6) = Val_long(nh); return Val_long(p - base); }
    if (b[p] == '\n') { p++; Field(v_out, 6) = Val_long(nh); return Val_long(p - base); }

    intnat name_off = p;
    while (p < end && b[p] != ':' && b[p] != '\n') p++;
    if (p >= end) return Val_long(PAW_INCOMPLETE);
    if (b[p] == '\n') {
      /* a line with no colon — tolerate it by skipping (matches the prior parser) */
      p++;
      continue;
    }
    intnat name_len = p - name_off; /* may be 0; harmless (won't match any lookup) */
    p++; /* skip ':' */

    /* value: skip leading OWS, read to line end, trim trailing OWS + the CR */
    while (p < end && paw_is_ows(b[p])) p++;
    if (p >= end) return Val_long(PAW_INCOMPLETE);
    intnat value_off = p;
    while (p < end && b[p] != '\n') p++;
    if (p >= end) return Val_long(PAW_INCOMPLETE);
    intnat value_end = p; /* at the '\n' */
    if (value_end > value_off && b[value_end - 1] == '\r') value_end--;
    while (value_end > value_off && paw_is_ows(b[value_end - 1])) value_end--;
    p++; /* skip '\n' */

    if (nh >= maxh) return Val_long(PAW_TOO_MANY);
    intnat o = 7 + 4 * nh;
    Field(v_out, o + 0) = Val_long(name_off);
    Field(v_out, o + 1) = Val_long(name_len);
    Field(v_out, o + 2) = Val_long(value_off);
    Field(v_out, o + 3) = Val_long(value_end - value_off);
    nh++;
  }
}
