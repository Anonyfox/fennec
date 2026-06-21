(* See port.mli. Who is listening on a TCP port, and whether a holder is our own dev server.

   The reclaim path SIGKILLs, so [is_ours] — the decision that gates the kill — is a pure, anchored
   predicate, unit-tested in isolation: it recognises our server binary as the PROGRAM being run
   (argv[0]), never a process that merely mentions the path as an argument (an editor, a grep). It
   errs toward NOT-ours; the cost of a miss is a leftover we name instead of auto-kill, vs. the cost
   of a false positive being SIGKILLing the wrong process. *)

(* the path from "_build/" onward (e.g. "_build/default/examples/site/server.bc"); the whole string
   if there is no "_build/". This build-relative tail is what we match, so a leftover started either
   absolutely (by a prior supervisor) or relatively is recognised by the same rule. *)
let build_tail exe =
  let needle = "_build/" in
  let ln = String.length exe and lk = String.length needle in
  let rec find i = if i + lk > ln then None else if String.sub exe i lk = needle then Some i else find (i + 1) in
  match find 0 with Some i -> String.sub exe i (ln - i) | None -> exe

let basename p = match String.rindex_opt p '/' with Some i -> String.sub p (i + 1) (String.length p - i - 1) | None -> p
let starts_with s pfx = let lp = String.length pfx in String.length s >= lp && String.sub s 0 lp = pfx

(* does token [t] denote our build artifact? its "_build/…" tail matches at a path boundary (start
   or '/'), so "…/myfake_build/…/server.bc" and a longer "…/notserver.bc" don't slip through *)
let is_artifact ~tail t =
  let lt = String.length tail and la = String.length t in
  lt > 0 && la >= lt && String.sub t (la - lt) lt = tail && (la = lt || t.[la - lt - 1] = '/')

(* `_build/<profile>/default/X` (the dev loop's ISOLATED build dir — see {!Build_dir}) -> `_build/
   default/X` (the form a plain `dune build` uses). The dev loop runs the server out of
   `_build/fastdev/default/…`, but a leftover may be the de-isolated `_build/default/…` (a plain build,
   or the test harness's default-built exe) — both are the SAME logical server, so reclaim must
   recognise either. Unchanged for a tail that is already on the default dir. *)
let deprofile tail =
  let p = "_build/" in
  if not (starts_with tail p) then tail
  else
    let rest = String.sub tail (String.length p) (String.length tail - String.length p) in
    match String.index_opt rest '/' with
    | Some i when String.sub rest 0 i <> "default" ->
      let after = String.sub rest (i + 1) (String.length rest - i - 1) in
      if starts_with after "default/" then p ^ after else tail
    | _ -> tail

let is_ours ~exe ~cmd =
  let t1 = build_tail exe in
  let t2 = deprofile t1 in
  (* match the holder against BOTH the dev loop's isolated tail and its de-isolated form, so a leftover
     from either build dir is ours. [is_artifact]'s path-boundary check is preserved, so a fake
     `…/myfake_build/…` still can't slip through. *)
  let our t = is_artifact ~tail:t1 t || (t2 <> t1 && is_artifact ~tail:t2 t) in
  match String.split_on_char ' ' (String.trim cmd) |> List.filter (fun s -> s <> "") with
  | [] -> false
  | argv0 :: rest ->
    (* our server is the PROGRAM being run: either argv[0] directly (a native exe, or a .bc with a
       custom runtime), or — the common bytecode case where ps shows "/…/ocamlrun _build/…/server.bc"
       — an argument of the OCaml runtime. Requiring an ocamlrun argv[0] for the argument case is
       what separates us from "vim _build/…/server.bc" / "sh -c …", where the path is merely an arg. *)
    our argv0 || (starts_with (basename argv0) "ocamlrun" && List.exists our rest)

(* ──── tests: is_ours ──── *)

let exe_ = "_build/default/examples/site/server.bc"

let%test "ocamlrun running our .bc is ours (abs runtime)" =
  is_ours ~exe:exe_ ~cmd:"/Users/x/.opam/5.4.1/bin/ocamlrun _build/default/examples/site/server.bc"

let%test "ocamlrun running our .bc is ours (bare runtime)" =
  is_ours ~exe:exe_ ~cmd:("ocamlrun " ^ exe_)

let%test "our relative path as argv0 is ours" =
  is_ours ~exe:exe_ ~cmd:exe_

let%test "our absolute path as argv0 is ours" =
  is_ours ~exe:exe_ ~cmd:"/Users/x/proj/_build/default/examples/site/server.bc"

let%test "our server with args after argv0 is ours" =
  is_ours ~exe:exe_ ~cmd:(exe_ ^ " --some-flag")

let%test "abs exe param matches a relatively-started holder" =
  is_ours ~exe:"/abs/proj/_build/default/examples/site/server.bc" ~cmd:exe_

let%test "exe without _build matches its full path as argv0" =
  is_ours ~exe:"/opt/app/server.bc" ~cmd:"/opt/app/server.bc"

let%test "vim editing the path is NOT ours (path is argv[1])" =
  not (is_ours ~exe:exe_ ~cmd:("vim " ^ exe_))

let%test "a different app's server is NOT ours" =
  not (is_ours ~exe:exe_ ~cmd:"_build/default/examples/admin/server.bc")

let%test "a foreign listener (python) is NOT ours" =
  not (is_ours ~exe:exe_ ~cmd:"python3 -m http.server 8200 --bind 127.0.0.1")

let%test "a non-'/'-boundary near-miss path is NOT ours" =
  not (is_ours ~exe:exe_ ~cmd:"/foo/myfake_build/default/examples/site/server.bc")

let%test "the bare path as argv[1] of a wrapper is NOT ours" =
  not (is_ours ~exe:exe_ ~cmd:("/bin/sh -c " ^ exe_))

let%test "empty command is NOT ours" =
  not (is_ours ~exe:exe_ ~cmd:"")

(* cross-build-dir: the dev loop runs the server from _build/fastdev/default/…, but a leftover (or the
   test harness's exe) may be the plain _build/default/… form — both are the same logical server *)
let fastdev_ = "/p/_build/fastdev/default/examples/site/server.bc"

let%test "a fastdev leftover is ours to a fastdev dev (the real-operation case)" =
  is_ours ~exe:fastdev_ ~cmd:("/x/ocamlrun " ^ fastdev_)

let%test "a DEFAULT leftover is ours to a FASTDEV dev (de-isolated form)" =
  is_ours ~exe:fastdev_ ~cmd:"/x/ocamlrun /p/_build/default/examples/site/server.bc"

let%test "a fastdev near-miss (myfake_build) is still NOT ours" =
  not (is_ours ~exe:fastdev_ ~cmd:"/foo/myfake_build/fastdev/default/examples/site/server.bc")

let%test "a different app is NOT ours even across build dirs" =
  not (is_ours ~exe:fastdev_ ~cmd:"/x/ocamlrun /p/_build/default/examples/admin/server.bc")

let listeners port =
  match (try Some (Unix.open_process_in (Printf.sprintf "lsof -nP -iTCP:%d -sTCP:LISTEN -t 2>/dev/null" port)) with _ -> None) with
  | None -> []
  | Some ic ->
    let pids = ref [] in
    (try
       while true do
         match int_of_string_opt (String.trim (input_line ic)) with Some p when p > 1 -> pids := p :: !pids | _ -> ()
       done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    List.map
      (fun p ->
        let cmd =
          match (try Some (input_line (Unix.open_process_in (Printf.sprintf "ps -p %d -o args= 2>/dev/null" p))) with _ -> None) with
          | Some s -> String.trim s
          | None -> ""
        in
        (p, cmd))
      !pids

let reclaim ~exe port =
  let mine = List.filter (fun (_, cmd) -> is_ours ~exe ~cmd) (listeners port) in
  List.iter (fun (pid, _) -> try Unix.kill pid Sys.sigkill with _ -> ()) mine;
  if mine <> [] then Unix.sleepf 0.2 (* let the port actually free before the retry binds *);
  mine <> []

let foreign_holder ~exe port = List.find_opt (fun (_, cmd) -> not (is_ours ~exe ~cmd)) (listeners port)
