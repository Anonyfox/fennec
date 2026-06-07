(* Error-panel system test (dev-only), ported from errors.sh. Provokes a build error through the
   REAL `fennec dev` and asserts the terminal does the right thing. Typed and contained (the
   System layer reaps the whole process group on teardown — no orphans between scenarios).

   Guards the exact bugs that bit us:
     1. the panel shows the RIGHT count (a syntax error's "might be unmatched" hint is the SAME
        error, not a second one) and an actual message;
     2. FIXING it clears the panel — even when the fix is a revert to byte-identical output, which
        dune rebuilds WITHOUT bumping the artifact mtime (the "stuck panel after a fix" bug). The
        supervisor must still notice the green build and print "resolved".

   `fennec dev`'s terminal output is captured by the System layer, so we grep it via wait_output /
   output instead of tailing a log file. *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let getenv k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d
let fennec = getenv "FENNEC_BIN" "fennec"
let app_dir = getenv "FENNEC_APP_DIR" (Sys.getcwd ())
let root = getenv "FENNEC_ROOT" (Filename.dirname (Filename.dirname app_dir))
let _ = root

let replace s ~old ~by =
  let n = String.length old and b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i + n <= String.length s && String.sub s !i n = old then (Buffer.add_string b by; i := !i + n)
    else (Buffer.add_char b s.[!i]; incr i)
  done; Buffer.contents b

let layout = Filename.concat app_dir "frontend/apps/web/layout.mlx"

let () = S.main @@ fun () ->

  S.test "error panel counts correctly + carries a message, and a revert-to-identical fix clears it" (fun sb ->
    let dev = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    (* gateway=4000 — reaching "ready" means it bound the port *)
    S.wait_ready dev ~port:4000 ();

    (* 1) provoke a syntax error (remove a delimiter), then 2) fix it by reverting to byte-identical
       output. with_edit injects the error and restores the file (the revert-to-identical case),
       so the panel-clears check runs AFTER with_edit's body. *)
    S.with_edit sb layout (fun s -> replace s ~old:"    ] />" ~by:"     />") (fun () ->
        S.wait_output dev ~timeout:30.0 "build failed";
        S.check "wrong error count (a multi-location syntax error must read as 1)"
          (contains (S.output dev) "build failed · 1 error");
        S.check "no error message shown for the syntax error"
          (contains (S.output dev) "Syntax error"));

    (* the supervisor must notice the green build (after a revert dune rebuilds without bumping the
       artifact mtime) and clear the stuck panel *)
    S.wait_output dev ~timeout:30.0 "resolved";
    S.check "the revert-to-identical fix cleared the panel" (contains (S.output dev) "resolved");

    S.signal dev Sys.sigint;
    S.wait_until ~timeout:10.0 (fun () -> not (S.alive dev)))
