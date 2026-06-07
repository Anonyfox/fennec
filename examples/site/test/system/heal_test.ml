(* `fennec dev --clean` system test (ported from heal.sh). The opt-in nuclear heal: --clean runs a
   full `dune clean` before starting (for the rare corrupt-_build case a normal restart can't fix),
   while a plain `fennec dev` leaves _build alone.

   These are [let%system_manual]: --clean WIPES the shared _build, which would delete the other
   suites' built artifacts mid-run, so they are SKIPPED by `fennec test system` and run only with
   `--manual`. Proven with a sentinel planted at the root of _build:
     - plain `fennec dev`   -> the sentinel SURVIVES (no clean);
     - `fennec dev --clean` -> the sentinel is GONE (dune clean ran) + the supervisor announces it. *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let sentinel () = Filename.concat (S.root ()) "_build/CLEAN_SENTINEL"
let page = 4001  (* dev port model: gateway=4000, web endpoint=4001 *)

let%system_manual "a plain start leaves _build alone (sentinel survives)" = fun sb ->
  let sentinel = sentinel () in
  S.write sb sentinel "";
  let dev = S.dev sb in
  S.wait_ready dev ~port:page ();
  S.check "a plain start wrongly cleaned _build" (S.exists sb sentinel);
  S.check "plain start didn't serve" (contains (S.request page "/").S.body "Welcome to the Fennec site");
  S.signal dev Sys.sigint;
  S.wait_until ~timeout:10.0 (fun () -> not (S.alive dev))

let%system_manual "--clean runs a full dune clean before starting (sentinel gone + announced)" = fun sb ->
  let sentinel = sentinel () in
  S.write sb sentinel "";
  let dev = S.dev sb ~args:[ "--clean" ] in
  (* wait only for the clean to land — the from-scratch rebuild that follows is slow *)
  S.wait_until ~timeout:30.0 (fun () -> not (S.exists sb sentinel));
  S.check "--clean did not run dune clean (sentinel still present)" (not (S.exists sb sentinel));
  S.wait_output dev ~timeout:30.0 "dune clean";
  S.check "--clean did not announce the clean" (contains (S.output dev) "dune clean");
  S.signal dev Sys.sigint;
  S.wait_until ~timeout:10.0 (fun () -> not (S.alive dev))
