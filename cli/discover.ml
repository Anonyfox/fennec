(* Find THE server in a dune project, with no config and no folder conventions.

   The identity of "the server" is not a filename or a directory — it is the single place that
   calls [Fennec.serve]. Any number of modules may LINK fennec (libraries, helpers, tests), but
   exactly one executable STARTS the server. So we ask dune for the project's executables and
   their own source files (via [dune describe], dune's supported machine interface), and pick the
   executable whose sources call [Fennec.serve]. Zero or more than one is an error with a clean
   message — the same invariant the runtime enforces ({!Fennec.serve} refuses a second call).

   Robustness: paths come from dune (not guessed), the scope is the cwd subtree (so running in an
   app dir finds that app), and the artifact path is derived from dune's own layout. The textual
   serve-call scan is a heuristic for discovery only; correctness is guaranteed at runtime. *)

(* ---- a tiny canonical-S-expression (csexp) reader: atoms are "<len>:<bytes>", lists "(...)" *)
type sexp = A of string | L of sexp list

let parse (s : string) : sexp =
  let pos = ref 0 and n = String.length s in
  let rec node () =
    if !pos >= n then failwith "csexp: unexpected end";
    match s.[!pos] with
    | '(' ->
      incr pos;
      let rec items acc =
        if !pos < n && s.[!pos] = ')' then (incr pos; L (List.rev acc)) else items (node () :: acc)
      in
      items []
    | c when c >= '0' && c <= '9' ->
      let start = !pos in
      while !pos < n && s.[!pos] <> ':' do incr pos done;
      let len = int_of_string (String.sub s start (!pos - start)) in
      incr pos;
      let a = String.sub s !pos len in
      pos := !pos + len;
      A a
    | c -> failwith (Printf.sprintf "csexp: unexpected %C" c)
  in
  node ()

(* ---- navigation helpers over the parsed tree ---- *)

(* every node shaped [(head . rest)], anywhere in the tree, as its [rest] *)
let find_records (head : string) (tree : sexp) : sexp list =
  let out = ref [] in
  let rec go t =
    (match t with L (A h :: r :: _) when h = head -> out := r :: !out | _ -> ());
    match t with L items -> List.iter go items | A _ -> ()
  in
  go tree;
  List.rev !out

(* the [rest] of the first [(key . rest)] field directly inside a record *)
let field (key : string) (record : sexp) : sexp list option =
  match record with
  | L fields -> List.find_map (function L (A k :: rest) when k = key -> Some rest | _ -> None) fields
  | _ -> None

let names_of rec_ =
  match field "names" rec_ with Some [ L l ] -> List.filter_map (function A x -> Some x | _ -> None) l | _ -> []

let module_impls rec_ : string list =
  match field "modules" rec_ with
  | Some [ L mods ] ->
    List.filter_map (fun m -> match field "impl" m with Some [ L [ A p ] ] -> Some p | _ -> None) mods
  | _ -> []

let root_of tree = match find_records "root" tree with A r :: _ -> Some r | _ -> None

(* ---- the search ---- *)

type t = { root : string; name : string; src_dir : string; exe : string; targets : string list }

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

(* strip OCaml [(* *)] comments (nested) and ["…"] string literals, so a mention of serve in a
   doc-comment or a manpage string is NOT mistaken for a call. A small scanner, not a full lexer,
   but enough to tell code from prose. *)
let strip_noise (s : string) : string =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 and depth = ref 0 and in_str = ref false in
  while !i < n do
    let c = s.[!i] in
    if !depth > 0 then
      if c = '(' && !i + 1 < n && s.[!i + 1] = '*' then (incr depth; i := !i + 2)
      else if c = '*' && !i + 1 < n && s.[!i + 1] = ')' then (decr depth; i := !i + 2)
      else incr i
    else if !in_str then
      if c = '\\' && !i + 1 < n then i := !i + 2
      else if c = '"' then (in_str := false; incr i)
      else incr i
    else if c = '(' && !i + 1 < n && s.[!i + 1] = '*' then (incr depth; i := !i + 2)
    else if c = '"' then (in_str := true; incr i)
    else if c = '\'' && !i + 2 < n && s.[!i + 1] <> '\\' && s.[!i + 2] = '\'' then i := !i + 3 (* 'x' *)
    else (Buffer.add_char b c; incr i)
  done;
  Buffer.contents b

(* does this source start a server? [Fennec.serve] is the qualified call; the aliased form
   ([open Fennec] then [serve …]) is accepted too. Scanned on code only (comments/strings
   stripped). Discovery heuristic — the runtime ({!Fennec.serve}) is the actual guarantee. *)
let calls_serve (src : string) : bool =
  let code = strip_noise src in
  contains code "Fennec.serve"
  || (contains code "open Fennec" && (contains code "serve [" || contains code "serve(" || contains code "serve\n"))

let strip_build p =
  let pfx = "_build/default/" in
  if contains p pfx && String.length p >= String.length pfx && String.sub p 0 (String.length pfx) = pfx then
    String.sub p (String.length pfx) (String.length p - String.length pfx)
  else p

let slurp_cmd (cmd : string) : string =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 65536 and chunk = Bytes.create 65536 in
  let rec go () =
    let k = input ic chunk 0 (Bytes.length chunk) in
    if k > 0 then (Buffer.add_subbytes buf chunk 0 k; go ())
  in
  go ();
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(* cwd relative to the workspace root ("" when at the root); None if cwd is outside root *)
let cwd_rel ~root =
  let cwd = Sys.getcwd () in
  if cwd = root then Some ""
  else
    let pfx = root ^ "/" in
    if String.length cwd > String.length pfx && String.sub cwd 0 (String.length pfx) = pfx then
      Some (String.sub cwd (String.length pfx) (String.length cwd - String.length pfx))
    else None

let under ~scope dir = scope = "" || dir = scope || (String.length dir > String.length scope && String.sub dir 0 (String.length scope + 1) = scope ^ "/")

(* find the single server executable in the cwd subtree, or a clean error message *)
let find () : (t, string) result =
  match (try Some (parse (slurp_cmd "dune describe --format csexp 2>/dev/null")) with _ -> None) with
  | None -> Error "could not run `dune describe` — is this a dune project, and is dune on PATH?"
  | Some tree -> (
    match root_of tree with
    | None -> Error "could not determine the dune workspace root"
    | Some root -> (
      match cwd_rel ~root with
      | None -> Error "current directory is outside the dune workspace"
      | Some scope ->
        (* every (server src-dir, exe name) whose source calls Fennec.serve, scoped to the cwd *)
        let servers =
          List.filter_map
            (fun rec_ ->
              let impls = module_impls rec_ |> List.map strip_build in
              let src_dir = match impls with p :: _ -> Filename.dirname p | [] -> "" in
              if not (under ~scope src_dir) then None
              else
                let serves =
                  List.exists
                    (fun rel ->
                      let abs = Filename.concat root rel in
                      match (try Some (In_channel.with_open_bin abs In_channel.input_all) with _ -> None) with
                      | Some src -> calls_serve src
                      | None -> false)
                    impls
                in
                if serves then Some (List.nth_opt (names_of rec_) 0, src_dir) else None)
            (find_records "executables" tree)
          (* one record can list several names; keep distinct (name, dir) servers *)
          |> List.filter_map (function Some n, d -> Some (n, d) | None, _ -> None)
          |> List.sort_uniq compare
        in
        match servers with
        | [] ->
          Error
            (Printf.sprintf
               "no server entrypoint found%s — exactly one executable must call Fennec.serve"
               (if scope = "" then "" else " under " ^ scope))
        | [ (name, src_dir) ] ->
          (* the BYTECODE exe: bytecode keeps dev cycles fast (no native codegen / C relinking
             per edit). Spawning a .bc directly needs CAML_LD_LIBRARY_PATH for its C stubs — the
             dev runner sets that (see Dev.run) so it doesn't depend on the parent shell. *)
          let exe = Printf.sprintf "%s/_build/default/%s/%s.bc" root src_dir name in
          (* the server's BYTECODE target. The caller adds the served web-root dir target (it
             knows its name, the [--assets] dir) — together they rebuild the SSR server AND the
             client bundle, but NOT the directory's native server.exe, whose ocamlopt pass is
             pure waste in a bytecode dev loop. *)
          let targets = [ Printf.sprintf "%s/%s.bc" src_dir name ] in
          Ok { root; name; src_dir; exe; targets }
        | many ->
          let listed = String.concat "\n" (List.map (fun (n, d) -> Printf.sprintf "  - %s (%s)" n d) many) in
          Error
            (Printf.sprintf
               "multiple server entrypoints found — only one place may start the server:\n%s\nstart \
                the server in exactly one executable, or pass the target explicitly."
               listed)))
