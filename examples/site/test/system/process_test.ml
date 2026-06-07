(* Process-hygiene + port-reclaim system tests for `fennec dev`, ported from no_leftovers.sh
   and reclaim.sh. Typed, deterministic (condition waits, never sleep-and-hope), and contained
   (the System layer reaps the whole process group on teardown — no orphans between scenarios).

   Config from the harness env: FENNEC_BIN (the fennec CLI), FENNEC_APP_DIR (the project to run
   `fennec dev` in), FENNEC_SERVER_BC (the built server, for the leftover-reclaim case). *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let getenv k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d
let fennec = getenv "FENNEC_BIN" "fennec"
let app_dir = getenv "FENNEC_APP_DIR" (Sys.getcwd ())
let server_bc = getenv "FENNEC_SERVER_BC" "_build/default/examples/site/server.bc"
let port = 4000

let () = S.main @@ fun () ->

  S.test "SIGKILL frees the port, restart binds, second reaps first, clean exit leaves nothing" (fun sb ->
    (* A — SIGKILL the supervisor (worst case: no cleanup handler runs); the port must free itself *)
    let s1 = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_ready s1 ~port ();
    S.check "port held while serving" (S.port_open port);
    S.signal s1 Sys.sigkill;   (* leader only — exercises fennec's OWN orphan safety net *)
    S.wait_until ~timeout:10.0 (fun () -> not (S.port_open port));
    S.check "port freed itself after SIGKILL" (not (S.port_open port));

    (* B — a fresh supervisor binds the freed port *)
    let s2 = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_ready s2 ~port ();
    S.check "fresh supervisor bound the port" (S.port_open port);

    (* C — a second supervisor reaps the first (single instance, no port fight) *)
    let s3 = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_ready s3 ~port ();
    S.wait_until ~timeout:10.0 (fun () -> not (S.alive s2));
    S.check "the previous supervisor was reaped by the new one" (not (S.alive s2));

    (* D — a clean shutdown (SIGINT) leaves nothing on the port *)
    S.signal s3 Sys.sigint;
    S.wait_until ~timeout:10.0 (fun () -> not (S.alive s3));
    S.wait_until ~timeout:10.0 (fun () -> not (S.port_open port));
    S.check "clean shutdown left nothing listening" (not (S.port_open port)));

  S.test "a leftover of our own server is auto-reclaimed" (fun sb ->
    (* a stray copy of OUR server holds :4000 *)
    let leftover = S.spawn sb ~cwd:app_dir ~env:[ ("FENNEC_ENV", "development") ] [ server_bc ] in
    S.wait_ready leftover ~port ();
    (* fennec dev starts; reaching "ready" means it bound the port — only possible by reclaiming
       the leftover that was holding it, which it does by SIGKILLing our own stray server *)
    let dev = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_output dev ~timeout:30.0 "ready";
    S.wait_until ~timeout:5.0 (fun () -> not (S.alive leftover));
    S.check "the leftover server was killed (reclaimed)" (not (S.alive leftover));
    S.check "the port is now served by fennec dev" (S.port_open port));

  S.test "a FOREIGN holder is named with a one-command fix, never killed" (fun sb ->
    (* a non-fennec process holds the port *)
    let foreign = S.spawn sb [ "python3"; "-m"; "http.server"; string_of_int port; "--bind"; "127.0.0.1" ] in
    S.wait_ready foreign ~port ();
    let foreign_pid = S.pid foreign in
    let dev = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_output dev ~timeout:30.0 "held by another process";
    S.check "the foreign process was NOT killed" (S.alive foreign);
    S.check "a one-command fix names the culprit" (contains (S.output dev) (Printf.sprintf "kill %d" foreign_pid)))
