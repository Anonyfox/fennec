(* `fennec dev --clean` system test (dev-only), ported from heal.sh. The opt-in nuclear heal:
   --clean runs a full `dune clean` before starting (for the rare corrupt-_build case a normal
   restart can't fix), while a plain `fennec dev` leaves _build alone. Typed and contained (the
   System layer reaps the whole process group on teardown — no orphans between scenarios).

   Proven with a sentinel planted at the root of _build:
     - plain `fennec dev`   -> the sentinel SURVIVES (no clean);
     - `fennec dev --clean` -> the sentinel is GONE (dune clean ran) + the supervisor announces it.

   NOTE: --clean WIPES _build, so the --clean case runs LAST and leaves a cold build behind —
   the next build rebuilds from scratch. For --clean we only wait for the clean to land, then
   stop; we do NOT sit through the slow from-scratch rebuild. *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let getenv k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d
let fennec = getenv "FENNEC_BIN" "fennec"
let app_dir = getenv "FENNEC_APP_DIR" (Sys.getcwd ())
let root = getenv "FENNEC_ROOT" (Filename.dirname (Filename.dirname app_dir))

let sentinel = Filename.concat root "_build/CLEAN_SENTINEL"
let page = 4001  (* dev port model: gateway=4000, web endpoint=4001 *)

let () = S.main @@ fun () ->

  S.test "a plain start leaves _build alone (sentinel survives)" (fun sb ->
    S.write sb sentinel "";
    let dev = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_ready dev ~port:page ();
    S.check "a plain start wrongly cleaned _build" (S.exists sb sentinel);
    S.check "plain start didn't serve" (contains (S.request page "/").S.body "Welcome to the Fennec site");
    S.signal dev Sys.sigint;
    S.wait_until ~timeout:10.0 (fun () -> not (S.alive dev)));

  (* runs LAST: --clean wipes _build and leaves a cold build behind *)
  S.test "--clean runs a full dune clean before starting (sentinel gone + announced)" (fun sb ->
    S.write sb sentinel "";
    let dev = S.spawn sb ~cwd:app_dir [ fennec; "dev"; "--clean" ] in
    (* wait only for the clean to land — the from-scratch rebuild that follows is slow *)
    S.wait_until ~timeout:30.0 (fun () -> not (S.exists sb sentinel));
    S.check "--clean did not run dune clean (sentinel still present)" (not (S.exists sb sentinel));
    S.wait_output dev ~timeout:30.0 "dune clean";
    S.check "--clean did not announce the clean" (contains (S.output dev) "dune clean");
    S.signal dev Sys.sigint;
    S.wait_until ~timeout:10.0 (fun () -> not (S.alive dev)))
