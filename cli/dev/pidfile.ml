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

let reap_stale ~cwd =
  match find_root cwd with
  | None -> ()
  | Some root -> (
    let path = path_for ~root in
    match read_all path with
    | None -> ()
    | Some body ->
      let pids = parse body in
      let killed = List.fold_left (fun acc pid -> if is_fennec_proc pid then (try Unix.kill pid Sys.sigkill; true with _ -> acc) else acc) false pids in
      (try Sys.remove path with _ -> ());
      (* give a killed dune daemon a moment to release its build lock *)
      if killed then Unix.sleepf 0.3)
