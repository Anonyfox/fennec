(* Proof that -data-client strips the inline server fetcher body. Both probe modules are force-linked
   (we reference a value from each); the SERVER build keeps the Server_only.fn body so the unique token
   is embedded in this binary, the -data-client build dropped it. We read our OWN binary and grep —
   the same string-literal-embedding mechanism examples/site/check_lean.ml uses for the prod-lean guard. *)

(* force both probe modules to link (else DCE could drop the one we never name). The fur ppx folds a
   module-level [let g = …]/[view] into the component's [make], so we reference [make] (the top-level
   binding) rather than [g]. *)
let () = ignore Strip_server.Probe.make
let () = ignore Strip_client.Probe.make

(* Assemble the search tokens at RUNTIME from fragments so the FULL literal is never embedded in THIS
   binary (else the runner's own search string would be a spurious extra match). [String.concat] is not
   constant-folded by the compiler, so neither full token appears in run_strip's compiled code. *)
let token = String.concat "" [ "FENNEC_STRIP"; "_PROBE_7Q" ]
let typed_token = String.concat "" [ "FENNEC_STRIP"; "_PROBE_TYPED_9Z" ]

(* allocation-light substring search over a multi-MB binary (verbatim shape from check_lean) *)
let contains hay ndl =
  let hl = String.length hay and nl = String.length ndl in
  if nl = 0 then true
  else
    let rec matches i j = j = nl || (hay.[i + j] = ndl.[j] && matches i (j + 1)) in
    let rec scan i = i + nl <= hl && (matches i 0 || scan (i + 1)) in
    scan 0

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else Sys.executable_name in
  let bytes = read_file path in
  (* SANITY: the server build MUST still carry the fetcher body (its token is present), proving the
     token-grep is meaningful — we are not measuring a token that was never compiled in the first place. *)
  if not (contains bytes token) then begin
    Printf.eprintf "strip-proof FAIL: server-build token %S absent — the grep is not meaningful\n" token;
    exit 1
  end;
  (* the actual strip proofs: NEITHER the string nor the typed token from the -data-client build's
     fetcher bodies may appear. Because both modules define the SAME token literal, a surviving client
     body would be indistinguishable from the server one — so we instead give the client fixture its OWN
     unique tokens? No: both probe.mlx are byte-identical (copy_files#). Distinguish by COUNT instead:
     with the client body stripped, the token appears for the SERVER module only. *)
  ignore typed_token;
  (* Count occurrences: the server build embeds each literal once; if the client body were NOT stripped,
     the SAME literal would appear an extra time. We assert each token appears AT MOST as many times as
     the server build alone would produce. The robust, copy_files#-compatible check is the typed token,
     which the .mlx uses ONLY inside a fetcher body (no view reference): it must appear exactly ONCE
     (server) — a second copy would mean the client body leaked. *)
  let count ndl =
    let hl = String.length bytes and nl = String.length ndl in
    let rec matches i j = j = nl || (bytes.[i + j] = ndl.[j] && matches i (j + 1)) in
    let rec go i acc = if i + nl > hl then acc else go (i + 1) (if matches i 0 then acc + 1 else acc) in
    go 0 0
  in
  let typed_n = count typed_token in
  if typed_n <> 1 then begin
    Printf.eprintf
      "strip-proof FAIL: typed fetcher token %S appears %d times (expected 1 — server only).\n\
      \  >1 means the -data-client build did NOT strip the inline fetcher body.\n"
      typed_token typed_n;
    exit 1
  end;
  Printf.printf
    "strip-proof OK: -data-client drops the inline Server_only.fn body (server keeps it: %S present; \
     typed fetcher token appears exactly once = server only, client stripped).\n"
    token
