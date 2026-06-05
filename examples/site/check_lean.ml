(* PROD-LEAN GUARANTEE (enforced).

   A production server links the runtime framework (Fennec) but must NEVER link the
   real-browser e2e/CDP test machinery (the separate `fennec-hunt` package) or its yojson
   dependency. That weight belongs in dev/test only. The separation is already structural —
   `server.exe` depends on `fennec`, which has no path to `fennec-hunt` — but "structural"
   silently regresses the day someone adds the wrong library to a server's deps, or a
   runtime module grows a dependency on it. This guard turns the invariant into a test.

   Mechanism: a native OCaml executable embeds each of its linked modules' names in the
   binary (verified: the e2e-linked `run.exe` contains "Fennec_hunt"/"Yojson" ~18×; the
   clean `server.exe` contains them 0×). So we read the built server binary and assert none
   of the forbidden module-name needles appear. Pure OCaml — no nm, no objdump, no shell,
   no platform-specific tooling — so it runs anywhere `dune runtest` does. *)

(* the module-name prefixes that may ONLY appear in a dev/test binary, never in prod:
   every e2e module is wrapped under [Fennec_hunt], and yojson is its (transitive) JSON dep *)
let forbidden = [ "Fennec_hunt"; "Yojson" ]

(* allocation-free substring search (the haystack is a multi-MB binary) *)
let contains hay ndl =
  let hl = String.length hay and nl = String.length ndl in
  if nl = 0 then true
  else begin
    let rec matches i j = j = nl || (hay.[i + j] = ndl.[j] && matches i (j + 1)) in
    let rec scan i = i + nl <= hl && (matches i 0 || scan (i + 1)) in
    scan 0
  end

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  if path = "" || not (Sys.file_exists path) then begin
    Printf.eprintf "check_lean: usage: check_lean <server-binary>  (got %S)\n" path;
    exit 2
  end;
  let bytes = read_file path in
  match List.filter (fun ndl -> contains bytes ndl) forbidden with
  | [] ->
    Printf.printf "prod-lean OK: %s links none of the e2e/CDP test machinery [%s]\n"
      (Filename.basename path) (String.concat ", " forbidden)
  | leaked ->
    Printf.eprintf
      "prod-lean FAIL: %s links forbidden dev/test machinery: %s\n\
      \  the production server must not carry the fennec-hunt / CDP / yojson weight.\n\
      \  check that no server depends on `fennec-hunt` (directly or transitively).\n"
      path (String.concat ", " leaked);
    exit 1
