(* Unit tests for Fennec_core.Http_semantics — content-encoding negotiation,
   conditional requests, Range parsing. Heavy on adversarial/edge inputs since
   this is the airtight HTTP layer. *)

module Sem = Fennec_core.Http_semantics

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let () =
  print_endline "negotiate_encoding:";
  let neg s = Sem.negotiate_encoding ~accept:(Some s) () in
  eq "absent -> identity" (Sem.negotiate_encoding ~accept:None ()) Sem.Identity;
  eq "empty -> identity" (neg "") Sem.Identity;
  eq "gzip" (neg "gzip") Sem.Gzip;
  eq "gzip,deflate -> gzip" (neg "gzip, deflate") Sem.Gzip;
  eq "deflate only" (neg "deflate") Sem.Deflate;
  eq "gzip;q=0 forbids" (neg "gzip;q=0, deflate") Sem.Deflate;
  eq "br unsupported -> identity" (neg "br") Sem.Identity;
  eq "ties prefer gzip" (neg "deflate;q=1.0, gzip;q=1.0") Sem.Gzip;
  eq "higher-q deflate wins" (neg "gzip;q=0.5, deflate;q=0.9") Sem.Deflate;
  eq "* enables gzip" (neg "*") Sem.Gzip;
  eq "gzip;q=0,* -> deflate" (neg "gzip;q=0, *") Sem.Deflate;
  (* adversarial / malformed *)
  eq "garbage q -> treated 1.0" (neg "gzip;q=banana") Sem.Gzip;
  eq "q clamped >1" (neg "gzip;q=5") Sem.Gzip;
  eq "negative q clamped to 0 (forbid)" (neg "gzip;q=-1, deflate") Sem.Deflate;
  eq "whitespace tolerant" (neg "  gzip ;  q=0.8 ") Sem.Gzip;
  eq "uppercase GZIP" (neg "GZIP") Sem.Gzip;
  eq "all forbidden -> identity" (neg "gzip;q=0, deflate;q=0") Sem.Identity;
  eq "identity;q=0 with nothing else -> identity (we don't error)" (neg "identity;q=0")
    Sem.Identity;
  eq "empty elements ignored" (neg ",,gzip,,") Sem.Gzip;

  print_endline "ETag / If-None-Match:";
  let etag = Sem.make_etag "abc123" in
  eq "quoted" etag "\"abc123\"";
  let inm v = [ ("If-None-Match", v) ] in
  check "exact" (Sem.if_none_match_satisfied ~etag (inm "\"abc123\""));
  check "star" (Sem.if_none_match_satisfied ~etag (inm "*"));
  check "weak matches strong" (Sem.if_none_match_satisfied ~etag (inm "W/\"abc123\""));
  check "in list" (Sem.if_none_match_satisfied ~etag (inm "\"x\", \"abc123\", \"y\""));
  check "no match" (not (Sem.if_none_match_satisfied ~etag (inm "\"other\"")));
  check "absent" (not (Sem.if_none_match_satisfied ~etag []));
  check "empty value" (not (Sem.if_none_match_satisfied ~etag (inm "")));
  check "case-insensitive header name"
    (Sem.if_none_match_satisfied ~etag [ ("IF-NONE-MATCH", "\"abc123\"") ]);

  print_endline "If-Modified-Since:";
  let d = Fennec_core.Http_date.format 1_000_000.0 in
  check "not modified (older resource)"
    (Sem.if_modified_since_satisfied ~mtime:999_000.0 [ ("If-Modified-Since", d) ]);
  check "modified (newer resource)"
    (not (Sem.if_modified_since_satisfied ~mtime:1_000_001.0 [ ("If-Modified-Since", d) ]));
  check "equal mtime -> not modified"
    (Sem.if_modified_since_satisfied ~mtime:1_000_000.0 [ ("If-Modified-Since", d) ]);
  check "garbage date -> false" (not (Sem.if_modified_since_satisfied ~mtime:0.0 [ ("If-Modified-Since", "xxx") ]));
  check "absent -> false" (not (Sem.if_modified_since_satisfied ~mtime:0.0 []));

  print_endline "Range parsing:";
  let r ?(len = 100) v = Sem.parse_range ~len [ ("Range", v) ] in
  eq "absent" (Sem.parse_range ~len:100 []) `None;
  eq "0-49" (r "bytes=0-49") (`Range { Sem.first = 0; last = 49 });
  eq "open end 50-" (r "bytes=50-") (`Range { Sem.first = 50; last = 99 });
  eq "suffix -20" (r "bytes=-20") (`Range { Sem.first = 80; last = 99 });
  eq "clamp last" (r "bytes=90-200") (`Range { Sem.first = 90; last = 99 });
  eq "start past end unsat" (r "bytes=100-200") `Unsatisfiable;
  eq "suffix 0 unsat" (r "bytes=-0") `Unsatisfiable;
  eq "multi-range declined" (r "bytes=0-1,5-6") `None;
  eq "non-bytes unit" (r "items=0-1") `None;
  eq "garbage" (r "bytes=abc") `None;
  eq "empty spec" (r "bytes=") `None;
  eq "just dash" (r "bytes=-") `None;
  eq "whole when first 0 last len-1" (r "bytes=0-99") (`Range { Sem.first = 0; last = 99 });
  eq "suffix larger than len -> whole" (r "bytes=-500") (`Range { Sem.first = 0; last = 99 });
  eq "missing bytes= prefix" (r "0-10") `None;
  eq "len 0 file, any range unsat" (r ~len:0 "bytes=0-0") `Unsatisfiable;

  if !fails = 0 then print_endline "all Http_semantics tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
