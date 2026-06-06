(* Discover + build the suite executables for a cut, by convention.

   Suites live at [<cwd>/test/http/*.ml] and [<cwd>/test/browser/*.ml] — each a plain dune
   executable using the fennec-hunt library. We glob the sources (so there's no list to
   maintain), build them with one `dune build` (fennec is the sole dune-aware process — no
   nested watcher, no lock deadlock), and resolve each to its built artifact.

   The path logic (cwd-relative-to-root, the _build artifact location) is pure and unit-tested;
   only the readdir + the dune invocation touch the world. *)

type t = {
  name : string;    (* suite name = source basename without .ml *)
  target : string;  (* dune build target, cwd-relative (e.g. "test/http/checkout.exe") *)
  exe : string;     (* absolute path to the built artifact *)
}

(* cwd expressed relative to the workspace root ("" when they are the same, or when cwd is not
   under root — in which case the caller is at the root) *)
let relativize ~root ~cwd =
  if cwd = root then ""
  else
    let r = root ^ "/" in
    if String.length cwd > String.length r && String.sub cwd 0 (String.length r) = r then
      String.sub cwd (String.length r) (String.length cwd - String.length r)
    else ""

(* the built artifact: <root>/_build/default/<reldir>/<dir>/<name>.exe (skipping empty parts) *)
let exe_path ~root ~reldir ~dir ~name =
  let parts = List.filter (fun s -> s <> "") [ "_build/default"; reldir; dir; name ^ ".exe" ] in
  Filename.concat root (String.concat "/" parts)

let build_target ~dir ~name = if dir = "" then name ^ ".exe" else dir ^ "/" ^ name ^ ".exe"

(* discover the suites in [<cwd>/<dir>] (sorted, deterministic); [] if the dir is absent *)
let discover ~root ~cwd ~dir : t list =
  let src_dir = Filename.concat cwd dir in
  if not (try Sys.is_directory src_dir with Sys_error _ -> false) then []
  else
    let reldir = relativize ~root ~cwd in
    Sys.readdir src_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".ml")
    |> List.sort compare
    |> List.map (fun f ->
           let name = Filename.chop_suffix f ".ml" in
           { name; target = build_target ~dir ~name; exe = exe_path ~root ~reldir ~dir ~name })

(* build the suite artifacts in one dune invocation; dune's errors surface to the user *)
let build (suites : t list) : (unit, string) result =
  if suites = [] then Ok ()
  else
    let cmd = "dune build " ^ String.concat " " (List.map (fun s -> Filename.quote s.target) suites) in
    match Sys.command cmd with
    | 0 -> Ok ()
    | n -> Error (Printf.sprintf "`dune build` failed (exit %d) — see the errors above" n)
