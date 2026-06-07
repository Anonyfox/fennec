(* Proves the System primitives: run, spawn, waits, fs, ports, and the signature invariant —
   teardown reaps the WHOLE process group, so a descendant the leader forked cannot orphan.
   Self-contained: only uses sh + sleep (universally available); no python, no fennec. *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let pid_alive pid = try Unix.kill pid 0; true with _ -> false

let () = S.main @@ fun () ->
  S.test "run captures output + exit 0" (fun sb ->
    let r = S.run sb [ "sh"; "-c"; "echo hello world" ] in
    S.check "exit 0" (r.S.status = Unix.WEXITED 0);
    S.check "output captured" (contains r.S.output "hello world"));

  S.test "run surfaces a non-zero exit" (fun sb ->
    let r = S.run sb [ "sh"; "-c"; "exit 3" ] in
    S.check "exit 3" (r.S.status = Unix.WEXITED 3));

  S.test "filesystem round-trip (creates parent dirs)" (fun sb ->
    S.write sb "sub/dir/file.txt" "content here";
    S.check "exists" (S.exists sb "sub/dir/file.txt");
    S.check "reads back" (S.read sb "sub/dir/file.txt" = "content here");
    S.rm sb "sub/dir/file.txt";
    S.check "gone after rm" (not (S.exists sb "sub/dir/file.txt")));

  S.test "free_port returns an unused port" (fun sb ->
    let port = S.free_port sb in
    S.check "above the privileged range" (port > 1024);
    S.check "nothing listening on it" (not (S.port_open port)));

  S.test "spawn / wait_output / alive / stop" (fun sb ->
    let p = S.spawn sb [ "sh"; "-c"; "echo up; while true; do sleep 1; done" ] in
    S.wait_output p ~timeout:5.0 "up";
    S.check "alive while running" (S.alive p);
    S.stop p;
    S.check "not alive after stop" (not (S.alive p)));

  S.test "wait_output raises Timeout on overrun" (fun sb ->
    let p = S.spawn sb [ "sh"; "-c"; "sleep 5" ] in
    let raised = try S.wait_output p ~timeout:0.3 "never happens"; false with S.Timeout _ -> true | _ -> false in
    S.check "Timeout raised" raised);

  S.test "teardown reaps the whole group (no orphaned descendant)" (fun sb ->
    (* the leader (sh) forks a long sleeper, records its pid, then waits *)
    let p = S.spawn sb [ "sh"; "-c"; "sleep 300 & echo $! > gc.pid; echo up; wait" ] in
    S.wait_output p ~timeout:5.0 "up";
    let descendant = int_of_string (String.trim (S.read sb "gc.pid")) in
    S.check "descendant alive before teardown" (pid_alive descendant);
    S.stop p;
    (* the group kill must have signalled the descendant; init reaps it promptly *)
    let rec reaped n = if not (pid_alive descendant) then true else if n <= 0 then false else (Unix.sleepf 0.05; reaped (n - 1)) in
    S.check "descendant reaped after group teardown" (reaped 40))
