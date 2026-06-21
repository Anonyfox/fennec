(* See pidfile.mli. *)

let parse (body : string) : int list =
  String.split_on_char '\n' body
  |> List.filter_map (fun line -> match int_of_string_opt (String.trim line) with Some p when p > 1 -> Some p | _ -> None)

let render (pids : int list) : string = String.concat "" (List.map (fun p -> string_of_int p ^ "\n") pids)

let rel = Filename.concat "_build" ".fennec_dev.pids"
let path_for ~root = Filename.concat root rel

let record path pids =
  (* write-then-rename so a crash mid-write can't leave the next run a torn file (rename is
     atomic on POSIX); dedup since the recorded tree overlaps across calls *)
  try
    let tmp = path ^ ".tmp" in
    let oc = open_out tmp in
    output_string oc (render (List.sort_uniq compare pids));
    close_out oc;
    Sys.rename tmp path
  with _ -> ()

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_root parent

let read_all path = try Some (In_channel.with_open_bin path In_channel.input_all) with _ -> None

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  ln = 0 || (let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in go 0)

(* the command name of a live pid via [ps], or None if it doesn't exist / ps is unavailable *)
let proc_comm pid =
  match (try Some (Unix.open_process_in (Printf.sprintf "ps -p %d -o comm= 2>/dev/null" pid)) with _ -> None) with
  | None -> None
  | Some ic ->
    let line = try Some (String.trim (input_line ic)) with _ -> None in
    ignore (Unix.close_process_in ic);
    (match line with Some "" | None -> None | x -> x)

let starts_with s pfx = let lp = String.length pfx in String.length s >= lp && String.sub s 0 lp = pfx
let ends_with s sfx = let ls = String.length s and lf = String.length sfx in ls >= lf && String.sub s (ls - lf) lf = sfx

(* is a process command name one of OURS (supervisor / dune / server / esbuild worker)? PURE — the
   identity gate, tested in isolation. Matched PRECISELY (exact name, "fennec" prefix, ".bc"
   suffix), never by loose substring: a recycled pid recorded under [_build] could otherwise be any
   process whose name merely CONTAINS "dune"/".bc" (e.g. "dunelike", "x.bcfg"), and the verdict
   gates a SIGKILL. *)
let comm_is_ours (comm : string) : bool =
  let b = Filename.basename comm in
  b = "dune" || b = "ocamlrun" || starts_with b "fennec" || ends_with b ".bc" || contains b "esbuild"

(* IS this pid still one of OUR processes? Pids recycle — and this file lives under [_build], so it
   can even outlive a reboot — so reaping by bare number could SIGKILL whatever unrelated process
   now holds the pid. Verifying the command name first ({!comm_is_ours}) makes that impossible. *)
let is_fennec_proc pid = match proc_comm pid with None -> false | Some name -> comm_is_ours name

(* A fennec-COMMAND process: the dev supervisor, or an internal worker that shares the command name
   (today only `fennec __esbuild-worker`). These are the ones we SIGTERM for a GRACEFUL takedown — the
   supervisor's own signal handler then stops its server (frees the port), its dune --watch (releases
   the build lock + leaves a CONSISTENT build dir, NOT a half-written one), its test worker and mongo,
   and removes this pidfile. The esbuild worker isn't auto-restarted, so signalling it directly is
   safe; we must NOT signal dune/server directly, as a still-live supervisor would just respawn them. *)
let comm_is_fennec (comm : string) : bool = starts_with (Filename.basename comm) "fennec"
let is_fennec_cli pid = match proc_comm pid with None -> false | Some name -> comm_is_fennec name

(* dune holds the build-dir lock; it frees the moment the process exits, so we identify dune procs to
   wait them out before the next `dune --watch` starts (else it queues behind a dying one). *)
let comm_is_dune (comm : string) : bool = Filename.basename comm = "dune"
let is_dune pid = match proc_comm pid with None -> false | Some name -> comm_is_dune name

(* cheap liveness probe (no [ps]) for the bounded wait loops; ESRCH ⇒ gone *)
let proc_alive pid = match Unix.kill pid 0 with () -> true | exception _ -> false

let reap_stale ~cwd =
  match find_root cwd with
  | None -> ()
  | Some root -> (
    let path = path_for ~root in
    match read_all path with
    | None -> ()
    | Some body ->
      let recorded = parse body in
      (* 1. GRACEFUL: ask the previous supervisor to shut itself (and its tree) down cleanly. *)
      let supervisors = List.filter is_fennec_cli recorded in
      List.iter (fun p -> try Unix.kill p Sys.sigterm with _ -> ()) supervisors;
      (* 2. wait for that clean exit — the handler removes this pidfile once it is DONE (server stopped,
         dune --watch fully shut down so the lock is free + the build dir consistent). Poll the FILE,
         not process liveness: a reaped child can linger as a zombie until ITS parent reaps it, which we
         are not, so a [kill 0] poll would over-wait. Bounded; a previous run that died without removing
         the file (a crash / kill -9) just falls through to the backstop below. *)
      if supervisors <> [] then begin
        let i = ref 0 in
        while !i < 20 && Sys.file_exists path do
          Unix.sleepf 0.1;
          incr i
        done
      end;
      (* 3. BACKSTOP: SIGKILL anything of OURS still alive — a hung supervisor, or orphans a previous
         `kill -9` left with no supervisor to reap them. The identity gate keeps a recycled pid (now an
         unrelated process) safe. *)
      List.iter (fun pid -> if is_fennec_proc pid then (try Unix.kill pid Sys.sigkill with _ -> ())) recorded;
      (try Sys.remove path with _ -> ());
      (* 4. wait out the dune procs so their build lock is actually free before the caller starts a new
         watch (replaces the old fixed 0.3s, which was both too long for a clean handoff and too short
         for a busy one). *)
      let dunes = List.filter is_dune recorded in
      if dunes <> [] then begin
        let i = ref 0 in
        while !i < 40 && List.exists proc_alive dunes do
          Unix.sleepf 0.05;
          incr i
        done
      end)
