(* The PRIMARY proof for the warm test worker — a direct drive of the worker over its socket, with NO
   dev server in the loop (that's what makes it reliable: a measurement loop against the live dev
   server would race the watcher; a direct drive does not).

   It proves, end to end:
     A. the example's test_components chain runs all 42 checks via the worker, at low single/double-digit
        ms (vs the ~320-450ms cold relink+launch baseline);
     B. a CROSS-FILE chain works (a dep .cma Dynlinked before the leaf module that uses it);
     C. CHANGED-MODULE reload across two requests to ONE worker — load Widget v1, then Widget v2 (same
        module name) — with no "already loaded" conflict, because each run forks;
     D. the FALLBACK fires: once the worker is killed, [run]/[healthy] return None/false so the caller
        can drop to the cold path.

   Artifacts are forced to exist by the rule's (deps …). dune runs this rule's action inside the build
   tree (cwd = …/_build/default/cli/test), so we anchor on the BUILD ROOT — the ancestor dir that
   actually contains cli/dev/worker — by walking up from cwd. (Walking to dune-project is wrong here:
   dune copies a dune-project into _build/default, so that walk stops one level too shallow.) *)

let build_root () =
  let anchor = "cli/dev/worker/fennec_test_worker.bc" in
  let rec up d =
    if Sys.file_exists (Filename.concat d anchor) then Some d
    else
      let p = Filename.dirname d in
      if p = d then None else up p
  in
  (* try the cwd-anchored build tree first; fall back to <dune-project root>/_build/default *)
  match up (Sys.getcwd ()) with
  | Some d -> d
  | None ->
    let rec proj d =
      if Sys.file_exists (Filename.concat d "dune-project") then d
      else let p = Filename.dirname d in if p = d then Sys.getcwd () else proj p
    in
    Filename.concat (proj (Sys.getcwd ())) "_build/default"

let bd = build_root ()
let bp rel = Filename.concat bd rel

let failures = ref 0
let check name ok = Printf.printf "%s %s\n%!" (if ok then "ok  " else "FAIL") name; if not ok then incr failures

let ok_lines output =
  String.split_on_char '\n' output
  |> List.filter (fun l -> let s = String.trim l in String.length s >= 2 && String.sub s 0 2 = "ok")
  |> List.length

let str_contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

(* ── the example test_components chain (dependency order, then the test .cmo) ──────────────────── *)
let example_chain =
  [ bp "examples/site/frontend/store/site_store.cma";
    bp "examples/site/frontend/components/site_components.cma";
    bp "examples/site/frontend/styles/site_styles.cma";
    bp "examples/site/frontend/handlers/site_handlers.cma";
    bp "examples/site/frontend_test/.test_components.eobjs/byte/dune__exe__Test_components.cmo" ]

let () =
  (* The worker dlopens the project C stubs (dllpaw_stubs, …) via CAML_LD_LIBRARY_PATH. Stublibs walks
     up from cwd to dune-project, but here cwd is inside the build tree (a dune-project is copied into
     _build/default), so we add the dll dirs explicitly from the build root [bd] we already resolved
     (plus the opam stublibs, which Stublibs handles). *)
  let add_stub_dir d =
    let cur = try Sys.getenv "CAML_LD_LIBRARY_PATH" with Not_found -> "" in
    if Sys.file_exists d && not (List.mem d (String.split_on_char ':' cur)) then
      Unix.putenv "CAML_LD_LIBRARY_PATH" (if cur = "" then d else d ^ ":" ^ cur)
  in
  List.iter (fun rel -> add_stub_dir (bp rel)) [ "paw"; "mongo/ffi"; "mongo/burrow/lmdb" ];
  Fennec_dev.Stublibs.ensure ();  (* adds the opam switch's stublibs *)
  (* resolve the worker binary deterministically *)
  Unix.putenv "FENNEC_TEST_WORKER" (bp "cli/dev/worker/fennec_test_worker.bc");

  (match Fennec_dev.Test_worker.spawn ~root:bd () with
   | None ->
     (* If the worker can't even spawn, the warm path is unprovable here — but that is itself the
        fallback contract, so fail loudly only if the binary IS present. *)
     check "worker spawns" (not (Sys.file_exists (bp "cli/dev/worker/fennec_test_worker.bc")));
     Printf.printf "worker binary missing — nothing to drive\n%!"
   | Some w ->
     check "worker spawns + handshakes" true;

     (* ── A. the example test, warm ─────────────────────────────────────────────────────────── *)
     (match Fennec_dev.Test_worker.run w ~chain:example_chain with
      | None -> check "A: example test_components runs via worker" false
      | Some r ->
        check "A: example exit code 0" (r.exit_code = 0);
        check "A: example ran all 42 checks" (ok_lines r.output = 42);
        check "A: example output is clean (no timing sentinel leak)" (not (str_contains r.output "FENNEC_TW_TIMING"));
        (* the warm cost must be a fraction of the ~320-450ms cold baseline — generous ceiling for CI *)
        check (Printf.sprintf "A: warm total %.1fms ≪ cold ~400ms (dynlink %.1f + run %.1f)" r.total_ms r.dynlink_ms r.run_ms)
          (r.total_ms < 200.0);
        Printf.printf "    [measured] example: total=%.1fms dynlink=%.1fms run=%.1fms (42 checks)\n%!"
          r.total_ms r.dynlink_ms r.run_ms);

     (* a second run of the SAME example chain in the same worker (warm-warm) must also be green *)
     (match Fennec_dev.Test_worker.run w ~chain:example_chain with
      | Some r -> check "A2: example re-run in same worker still 42 ok" (r.exit_code = 0 && ok_lines r.output = 42)
      | None -> check "A2: example re-run in same worker" false);

     (* ── B. cross-file chain (dep .cma before the leaf that references it) ──────────────────── *)
     let chain_v1 =
       [ bp "cli/test/wwfix/dep/wwfix_dep.cma"; bp "cli/test/wwfix/v1/.wwfix_v1.objs/byte/widget.cmo" ]
     in
     (match Fennec_dev.Test_worker.run w ~chain:chain_v1 with
      | None -> check "B: cross-file chain runs" false
      | Some r ->
        check "B: cross-file chain exit 0" (r.exit_code = 0);
        check "B: leaf saw the dep's marker (dep loaded first)" (str_contains r.output "WW:marker");
        check "B: ran v1" (str_contains r.output "v1"));

     (* ── C. changed-module reload: Widget v1 then Widget v2 in the SAME worker ──────────────── *)
     let chain_v2 =
       [ bp "cli/test/wwfix/dep/wwfix_dep.cma"; bp "cli/test/wwfix/v2/.wwfix_v2.objs/byte/widget.cmo" ]
     in
     (match Fennec_dev.Test_worker.run w ~chain:chain_v2 with
      | None -> check "C: reload v2 runs" false
      | Some r ->
        check "C: reload v2 exit 0 (no module-already-loaded conflict)" (r.exit_code = 0);
        check "C: v2 body actually ran (fresh bytes, not the cached v1)" (str_contains r.output "v2"));

     (* ── D. fallback: kill the worker, then run/healthy must give up cleanly ────────────────── *)
     (try Unix.kill (Fennec_dev.Test_worker.pid w) Sys.sigkill with _ -> ());
     Unix.sleepf 0.2;
     check "D: healthy is false after the worker is killed" (not (Fennec_dev.Test_worker.healthy w));
     check "D: run returns None after the worker is killed (→ caller falls back)"
       (Fennec_dev.Test_worker.run w ~chain:example_chain = None);

     Fennec_dev.Test_worker.shutdown w);

  if !failures = 0 then (Printf.printf "all warm-worker drive checks passed.\n%!"; exit 0)
  else (Printf.printf "%d warm-worker drive checks FAILED.\n%!" !failures; exit 1)
