(* Stage jsoo bundles into the served tree — a NATIVE replacement for a bash glob-loop.
   Globs the cwd: <name>.bc.js -> served/<prefix>/<name>/main.js, <name>.css -> .../main.css.
   One process, in-process byte copies — no per-file basename/mkdir/cp fork (the bash version
   spent ~24ms almost entirely on those forks; this is one exec + a few writes).

   dune runs it in the bundle dir under a (rule (targets (dir served)) (action (run stage <prefix>))).
   The (dir served) target still re-stages every bundle on any change — dune forbids per-bundle
   targets here (no (subdir) or subdir-target inside a (dynamic_include)) — but the cost is now a
   single native exec, not a fork storm. usage: stage <prefix>   (_apps | _handlers) *)

let read f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write f s =
  let oc = open_out_bin f in
  output_string oc s;
  close_out oc

let rec mkdirs d =
  if d <> "" && d <> "." && d <> "/" && not (Sys.file_exists d) then begin
    mkdirs (Filename.dirname d);
    (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let () =
  let prefix =
    match Array.to_list Sys.argv with
    | _ :: p :: _ -> p
    | _ -> prerr_endline "usage: stage <prefix>"; exit 2
  in
  (* always materialise served/<prefix>/ so the (dir served) target exists even with no bundles *)
  mkdirs (Filename.concat "served" prefix);
  let out name target =
    let dir = Filename.concat (Filename.concat "served" prefix) name in
    mkdirs dir;
    Filename.concat dir target
  in
  Sys.readdir "."
  |> Array.iter (fun f ->
         if Filename.check_suffix f ".bc.js" then write (out (Filename.chop_suffix f ".bc.js") "main.js") (read f)
         else if Filename.check_suffix f ".css" then write (out (Filename.chop_suffix f ".css") "main.css") (read f))
