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

(* the module-name prefixes that may ONLY appear in a dev/test binary, never in prod.
   Fennec_hunt_cdp / Fennec_hunt_chrome / Fennec_hunt_http_client are the heavy e2e machinery
   (Chrome CDP, TLS, websockets). Yojson is their transitive JSON dep.
   Fennec_hunt_unit is ALLOWED: it's the lightweight inline-test runtime (1KB, dep: unix only),
   so libraries can carry let%test registrations. The registered thunks are inert in production
   (nobody calls Unit.run). *)
let forbidden = [ "Fennec_hunt__Cdp"; "Fennec_hunt__Chrome"; "Fennec_hunt__Http_client"; "Yojson" ]

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
    Printf.printf "prod-lean OK: %s links none of the heavy test machinery [%s]\n"
      (Filename.basename path) (String.concat ", " forbidden)
  | leaked ->
    Printf.eprintf
      "prod-lean FAIL: %s links heavy test machinery: %s\n\
      \  the production server must not carry the CDP/Chrome/yojson test weight.\n\
      \  check that no server depends on `fennec-hunt` (directly or transitively).\n\
      \  (fennec-hunt.unit is OK — it's the 1KB inline-test runtime, not the heavy layer.)\n"
      path (String.concat ", " leaked);
    exit 1
