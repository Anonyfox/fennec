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

let is_ours ~exe ~cmd =
  let tail = build_tail exe in
  match String.split_on_char ' ' (String.trim cmd) |> List.filter (fun s -> s <> "") with
  | [] -> false
  | argv0 :: rest ->
    (* our server is the PROGRAM being run: either argv[0] directly (a native exe, or a .bc with a
       custom runtime), or — the common bytecode case where ps shows "/…/ocamlrun _build/…/server.bc"
       — an argument of the OCaml runtime. Requiring an ocamlrun argv[0] for the argument case is
       what separates us from "vim _build/…/server.bc" / "sh -c …", where the path is merely an arg. *)
    is_artifact ~tail argv0 || (starts_with (basename argv0) "ocamlrun" && List.exists (is_artifact ~tail) rest)

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
