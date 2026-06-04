(* See pidfile.mli. *)

let parse (body : string) : int list =
  String.split_on_char '\n' body
  |> List.filter_map (fun line -> match int_of_string_opt (String.trim line) with Some p when p > 1 -> Some p | _ -> None)

let render (pids : int list) : string = String.concat "" (List.map (fun p -> string_of_int p ^ "\n") pids)

let rel = Filename.concat "_build" ".fennec_dev.pids"
let path_for ~root = Filename.concat root rel

let record path pids =
  try
    let oc = open_out path in
    output_string oc (render pids);
    close_out oc
  with _ -> ()

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_root parent

let read_all path = try Some (In_channel.with_open_bin path In_channel.input_all) with _ -> None

let reap_stale ~cwd =
  match find_root cwd with
  | None -> ()
  | Some root -> (
    let path = path_for ~root in
    match read_all path with
    | None -> ()
    | Some body ->
      let pids = parse body in
      let killed = List.fold_left (fun acc pid -> (try Unix.kill pid Sys.sigkill; true with _ -> acc)) false pids in
      (try Sys.remove path with _ -> ());
      (* give a killed dune daemon a moment to release its build lock *)
      if killed then Unix.sleepf 0.3)
