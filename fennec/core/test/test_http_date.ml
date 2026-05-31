(* Unit tests for Fennec_core.Http_date — RFC 7231 IMF-fixdate format/parse plus
   the two obsolete forms. Edge cases: round-trip, all three formats, garbage,
   epoch boundaries, leap-ish dates. *)

module Date = Fennec_core.Http_date

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let () =
  print_endline "format:";
  (* the canonical RFC 7231 example: 784111777 = Sun, 06 Nov 1994 08:49:37 GMT *)
  eq "RFC example format" (Date.format 784_111_777.0) "Sun, 06 Nov 1994 08:49:37 GMT";
  eq "epoch 0" (Date.format 0.0) "Thu, 01 Jan 1970 00:00:00 GMT";
  check "length 29" (String.length (Date.format 1_700_000_000.0) = 29);

  print_endline "parse:";
  eq "IMF" (Date.parse "Sun, 06 Nov 1994 08:49:37 GMT") (Some 784_111_777.0);
  eq "asctime" (Date.parse "Sun Nov  6 08:49:37 1994") (Some 784_111_777.0);
  eq "rfc850" (Date.parse "Sunday, 06-Nov-94 08:49:37 GMT") (Some 784_111_777.0);
  eq "epoch parse" (Date.parse "Thu, 01 Jan 1970 00:00:00 GMT") (Some 0.0);
  eq "garbage" (Date.parse "not a date") None;
  eq "empty" (Date.parse "") None;
  eq "partial" (Date.parse "Sun, 06 Nov") None;
  eq "bad month" (Date.parse "Sun, 06 Xyz 1994 08:49:37 GMT") None;
  eq "leading/trailing ws tolerated" (Date.parse "  Sun, 06 Nov 1994 08:49:37 GMT  ")
    (Some 784_111_777.0);

  print_endline "round-trip:";
  let ts = [ 0.0; 1.0; 784_111_777.0; 1_700_000_000.0; 2_000_000_000.0 ] in
  List.iter
    (fun t -> eq (Printf.sprintf "rt %.0f" t) (Date.parse (Date.format t)) (Some t))
    ts;
  (* fractional seconds: format truncates, parse returns the whole second *)
  eq "fractional truncates" (Date.parse (Date.format 100.9)) (Some 100.0);

  if !fails = 0 then print_endline "all Http_date tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
