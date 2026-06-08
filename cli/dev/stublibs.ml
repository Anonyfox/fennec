(* Make a directly-spawned bytecode server able to dlopen its C stubs (e.g. the mongo driver's
   libmongoc stub) by putting the right dirs on CAML_LD_LIBRARY_PATH — what `dune exec` / `opam env`
   would otherwise provide. Two sources:

   - each PROJECT-LOCAL lib's own [_build/default] dir, where dune builds its [dll*.so]. This is
     present after ANY build that links the stub — including the targeted `fennec test` / `fennec
     dev` build of the server (the install-staging dir, by contrast, is only populated by a full /
     `@install` build, so we do NOT rely on it). Found by a bounded directory walk, so [ensure] must
     run AFTER the build / at spawn time for the dirs to exist.
   - the opam switch's stublibs (installed deps' stubs).

   Idempotent (a dir already on the path is not re-added); [putenv] mutates this process's
   environment, which spawned children inherit. *)

let opam_stublibs () =
  match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
  | Some p when p <> "" -> Some (Filename.concat p "lib/stublibs")
  | _ -> (
    match (try Some (String.trim (input_line (Unix.open_process_in "opam var lib 2>/dev/null"))) with _ -> None) with
    | Some lib when lib <> "" -> Some (Filename.concat lib "stublibs")
    | _ -> None)

(* the dune-project root: the cwd, or an ancestor holding [dune-project] (a monorepo example is run
   from a subdir while [_build] sits at the ancestor) *)
let project_root () =
  let rec up d =
    if Sys.file_exists (Filename.concat d "dune-project") then d
    else
      let p = Filename.dirname d in
      if p = d then Sys.getcwd () else up p
  in
  up (Sys.getcwd ())

let is_dll e = String.length e > 3 && String.sub e 0 3 = "dll" && Filename.check_suffix e ".so"

(* dirs under [_build/default] that hold a [dll*.so] (bounded walk; one-time per call) *)
let project_stub_dirs () =
  let acc = ref [] in
  let rec walk depth dir =
    if depth <= 8 then
      match Sys.readdir dir with
      | entries ->
        if Array.exists is_dll entries then acc := dir :: !acc;
        Array.iter (fun e -> let p = Filename.concat dir e in if (try Sys.is_directory p with _ -> false) then walk (depth + 1) p) entries
      | exception _ -> ()
  in
  walk 0 (Filename.concat (project_root ()) "_build/default");
  !acc

let ensure () =
  let add dir =
    let cur = try Sys.getenv "CAML_LD_LIBRARY_PATH" with Not_found -> "" in
    if dir <> "" && not (List.mem dir (String.split_on_char ':' cur)) then
      Unix.putenv "CAML_LD_LIBRARY_PATH" (if cur = "" then dir else dir ^ ":" ^ cur)
  in
  List.iter add (project_stub_dirs ());
  match opam_stublibs () with Some d -> add d | None -> ()
