(* Derive a dev-loop test's APP-LOCAL Dynlink chain — the ordered list of bytecode objects the warm
   test worker loads per edit: the test's local library [.cma]s in dependency order, then the test
   module's own [.cmo]. Everything else (the stable framework) is already linked into the worker, so
   it is NOT in the chain; it only has to be CLASSIFIABLE as preloaded — if a framework dep the worker
   did not preload turns up, we refuse to warm-path the test and the caller falls back to a cold run.

   The graph comes from [dune describe workspace] (the principled, versioned-when-asked source): every
   library entry carries [name]/[uid]/[local]/[requires]/[source_dir]. Public-named libs appear under
   their PUBLIC name there (e.g. [fennec.fur]), which is exactly the name a dune [(libraries …)] field
   uses — so the test stanza's deps map straight onto describe entries with no translation.

   [(test)] stanzas are NOT emitted by describe, so the test's OWN direct lib deps are read from its
   [dune] file (parsed by the caller and passed in). From there it is a pure graph walk:
     1. transitive closure of [requires] from the test's direct lib uids;
     2. partition the closure into local-under-a-watched-root (→ Dynlink) vs framework. A PROJECT-LOCAL
        framework lib must be in the worker's preload set or we refuse (pre-emptive fallback); opam
        libs are assumed present (they ride in transitively with the project framework the worker
        links, and the rare genuinely-new opam dep is caught by the worker's RUNTIME Dynlink error →
        fallback). That split keeps the preload set small + easy to keep in sync with the worker dune;
     3. topologically order the local libs (deps before dependents) and map each to
        [<source_dir>/<name>.cma];
     4. append the test's own [.cmo].

   Pure given the describe text + inputs, so the whole thing is unit-tested on a synthetic describe
   fixture below. *)

(* ── a tiny S-expression reader (the describe output is large but structurally trivial) ───────── *)

type sexp = Atom of string | List of sexp list

let parse_sexp s =
  let n = String.length s in
  let pos = ref 0 in
  let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r' in
  let skip () = while !pos < n && is_ws s.[!pos] do incr pos done in
  let read_atom () =
    let b = Buffer.create 32 in
    while !pos < n && (not (is_ws s.[!pos])) && s.[!pos] <> '(' && s.[!pos] <> ')' do
      Buffer.add_char b s.[!pos];
      incr pos
    done;
    Atom (Buffer.contents b)
  in
  let rec read () =
    skip ();
    if !pos >= n then Atom ""
    else if s.[!pos] = '(' then begin
      incr pos;
      let items = ref [] in
      skip ();
      while !pos < n && s.[!pos] <> ')' do
        items := read () :: !items;
        skip ()
      done;
      if !pos < n then incr pos;
      List (List.rev !items)
    end
    else read_atom ()
  in
  read ()

(* ── library facts pulled from describe ───────────────────────────────────────────────────────── *)

type lib = {
  name : string;
  uid : string;
  local : bool;
  requires : string list;  (* dep uids *)
  source_dir : string;     (* e.g. _build/default/examples/site/frontend/store *)
}

let field name = function
  | List items ->
    List.find_map (function List (Atom k :: rest) when k = name -> Some rest | _ -> None) items
  | _ -> None

let atom1 = function Some [ Atom a ] -> a | _ -> ""

(* a (requires (a b c)) field, or a flat (requires a b c) — accept both shapes *)
let atoms = function
  | Some [ List l ] -> List.filter_map (function Atom a -> Some a | _ -> None) l
  | Some l -> List.filter_map (function Atom a -> Some a | _ -> None) l
  | None -> []

(* parse the whole describe doc into its [library] entries (executables/dirs ignored) *)
let libs_of_describe text =
  match parse_sexp text with
  | List items ->
    List.filter_map
      (function
        | List [ Atom "library"; body ] ->
          Some
            { name = atom1 (field "name" body);
              uid = atom1 (field "uid" body);
              local = atom1 (field "local" body) = "true";
              requires = atoms (field "requires" body);
              source_dir = atom1 (field "source_dir" body) }
        | _ -> None)
      items
  | _ -> []

(* ── classification ───────────────────────────────────────────────────────────────────────────── *)

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

(* a local lib whose source sits under one of the watched roots is a Dynlink candidate; any other lib
   (project-local framework, or opam) is "framework" and must already be in the worker. The build dir
   prefix is stripped so we compare against the workspace-relative watched roots. *)
(* the default build-context prefix (the [dev] profile / a synthetic describe in the unit tests). The
   real dev loop passes the per-profile prefix from {!Build_dir.describe_prefix} (absolute under an
   isolated [fastdev] build dir), so [derive] takes [?build_prefix]. *)
let default_build_prefix = "_build/default/"

let under_watched ~build_prefix ~watch_roots (l : lib) =
  if not l.local then false
  else
    let rel =
      if starts_with ~prefix:build_prefix l.source_dir then
        String.sub l.source_dir (String.length build_prefix)
          (String.length l.source_dir - String.length build_prefix)
      else l.source_dir
    in
    List.exists (fun root -> rel = root || starts_with ~prefix:(root ^ "/") rel) watch_roots

(* ── the derivation ───────────────────────────────────────────────────────────────────────────── *)

type t = {
  cmas : string list;  (* local lib archives, dependency order (deps first), workspace-relative *)
  cmo : string;        (* the test module's own object, workspace-relative *)
}
(** A derived warm-path chain: [cmas] (loaded first, in order) then [cmo]. *)

type error =
  | Unknown_lib of string        (* a declared test dep isn't in the describe graph *)
  | Needs_unpreloaded of string  (* a framework dep the worker did not preload — can't warm-path *)
  | No_describe                  (* describe yielded no libraries (build not ready / parse miss) *)

let error_to_string = function
  | Unknown_lib l -> Printf.sprintf "library %S is not in the dune graph" l
  | Needs_unpreloaded l -> Printf.sprintf "framework lib %S is not preloaded in the worker" l
  | No_describe -> "dune describe produced no libraries"

(* workspace-relative .cma for a local lib: strip the _build/default/ prefix off source_dir, append
   <name>.cma. (The worker resolves paths against the build root, so relative is enough + portable.) *)
let cma_of ~build_prefix (l : lib) =
  let dir =
    if starts_with ~prefix:build_prefix l.source_dir then
      String.sub l.source_dir (String.length build_prefix)
        (String.length l.source_dir - String.length build_prefix)
    else l.source_dir
  in
  Filename.concat dir (l.name ^ ".cma")

(* the test module's own .cmo, from the workspace-relative exe target (e.g.
   examples/site/frontend_test/test_components.exe →
   examples/site/frontend_test/.test_components.eobjs/byte/dune__exe__Test_components.cmo).
   The dune__exe__ prefix + Capitalized module is dune's executable-module mangling. *)
let cmo_of_target target =
  let dir = Filename.dirname target in
  let base = Filename.remove_extension (Filename.basename target) in
  let modname = String.capitalize_ascii base in
  Filename.concat
    (Filename.concat dir (Printf.sprintf ".%s.eobjs" base))
    (Filename.concat "byte" (Printf.sprintf "dune__exe__%s.cmo" modname))

(* [derive ~describe ~watch_roots ~preloaded ~test_libs ~target]:
   - [describe]    : the [dune describe workspace] text
   - [watch_roots] : workspace-relative dirs whose local libs are Dynlinked (e.g. ["examples/site"])
   - [preloaded]   : framework lib names the worker linked (-linkall); membership decides warm vs cold
   - [test_libs]   : the test stanza's direct [(libraries …)] names
   - [target]      : the test exe's workspace-relative target (…/<name>.exe) *)
let derive ?(build_prefix = default_build_prefix) ~describe ~watch_roots ~preloaded ~test_libs ~target () =
  let libs = libs_of_describe describe in
  if libs = [] then Error No_describe
  else begin
    let by_uid = Hashtbl.create 256 in
    let by_name = Hashtbl.create 256 in
    List.iter
      (fun l ->
        Hashtbl.replace by_uid l.uid l;
        Hashtbl.replace by_name l.name l)
      libs;
    let preloaded_set = Hashtbl.create 64 in
    List.iter (fun n -> Hashtbl.replace preloaded_set n ()) preloaded;
    (* seeds: the test's direct lib deps, mapped to graph entries *)
    let seeds = List.map (fun n -> (n, Hashtbl.find_opt by_name n)) test_libs in
    match List.find_opt (fun (_, o) -> o = None) seeds with
    | Some (n, _) -> Error (Unknown_lib n)
    | None ->
      let seed_libs = List.map (fun (_, o) -> Option.get o) seeds in
      (* check every framework lib in the transitive closure is preloaded; collect local-watched libs
         in dependency order via post-order DFS (a lib is emitted after the local deps it needs). *)
      let visiting = Hashtbl.create 256 in
      let done_ = Hashtbl.create 256 in
      let ordered = ref [] in  (* reverse post-order accumulator of local-watched libs *)
      let err = ref None in
      let rec visit (l : lib) =
        if !err = None && not (Hashtbl.mem done_ l.uid) && not (Hashtbl.mem visiting l.uid) then begin
          Hashtbl.replace visiting l.uid ();
          (* descend into deps first *)
          List.iter
            (fun dep_uid ->
              match Hashtbl.find_opt by_uid dep_uid with
              | Some dl -> visit dl
              | None -> () (* a dep with no describe entry (e.g. a pp-only/opam edge) — not loadable, skip *))
            l.requires;
          Hashtbl.remove visiting l.uid;
          Hashtbl.replace done_ l.uid ();
          if under_watched ~build_prefix ~watch_roots l then ordered := l :: !ordered
          else if l.local && not (Hashtbl.mem preloaded_set l.name) && !err = None then
            (* a PROJECT-LOCAL framework lib the worker did not link — pre-emptively refuse. (opam libs
               are not checked here: they ride into the worker transitively with the project framework
               it links, and the rare genuinely-new opam dep is caught by the worker's runtime Dynlink
               error → fallback. Checking only project libs keeps the preload set small + in sync.) *)
            err := Some (Needs_unpreloaded l.name)
        end
      in
      List.iter visit seed_libs;
      (match !err with
       | Some e -> Error e
       | None ->
         let cmas = List.rev_map (cma_of ~build_prefix) !ordered in
         Ok { cmas; cmo = cmo_of_target target })
  end

(* the full ordered object list the worker loads: cmas (deps first) then the test cmo *)
let objects t = t.cmas @ [ t.cmo ]

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Tests — pure derivation over a synthetic describe fixture                 *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

(* a miniature describe doc mirroring the example: 4 local app libs under examples/site (styles is a
   leaf, store is a leaf, components needs store, handlers needs components + a framework lib), two
   project-local framework libs (fennec.fur, fennec), and one external opam lib (fmt, pulled in by
   fennec.fur). uids are human-readable here. *)
let fixture =
  {|((root /x)
 (build_context _build/default)
 (library
  ((name site_styles) (uid u_styles) (local true) (requires ())
   (source_dir _build/default/examples/site/frontend/styles)))
 (library
  ((name site_store) (uid u_store) (local true) (requires (u_fur))
   (source_dir _build/default/examples/site/frontend/store)))
 (library
  ((name site_components) (uid u_comp) (local true) (requires (u_store u_fur))
   (source_dir _build/default/examples/site/frontend/components)))
 (library
  ((name site_handlers) (uid u_hand) (local true) (requires (u_comp u_fennec))
   (source_dir _build/default/examples/site/frontend/handlers)))
 (library
  ((name fennec.fur) (uid u_fur) (local true) (requires (u_fmt))
   (source_dir _build/default/fennec/fur)))
 (library
  ((name fennec) (uid u_fennec) (local true) (requires (u_fur))
   (source_dir _build/default/fennec)))
 (library
  ((name fmt) (uid u_fmt) (local false) (requires ())
   (source_dir /opam/lib/fmt)))
 (executables ((names (snapshot_html)) (requires (u_fur)) (modules ()))))|}

let test_target = "examples/site/frontend_test/test_components.exe"
let watch_roots = [ "examples/site" ]
let preloaded = [ "fennec.fur"; "fennec" ]

let%test "chain: local app libs in dep order, framework skipped, test cmo last" =
  match
    derive ~describe:fixture ~watch_roots ~preloaded
      ~test_libs:[ "fennec.fur"; "site_components"; "site_store"; "site_styles"; "site_handlers" ]
      ~target:test_target ()
  with
  | Error _ -> false
  | Ok t ->
    (* store before components before handlers; styles present; the test cmo last; no framework cma *)
    let cmas = t.cmas in
    let idx name = List.find_index (fun p -> Fennec_hunt_unit.str_contains p name) cmas in
    let i_store = idx "site_store" and i_comp = idx "site_components" and i_hand = idx "site_handlers" in
    let ordered =
      match (i_store, i_comp, i_hand) with
      | Some a, Some b, Some c -> a < b && b < c
      | _ -> false
    in
    ordered
    && idx "site_styles" <> None
    && not (List.exists (fun p -> Fennec_hunt_unit.str_contains p "fennec/fur") cmas)
    && t.cmo = "examples/site/frontend_test/.test_components.eobjs/byte/dune__exe__Test_components.cmo"

let%test "chain: .cma path is <rel source_dir>/<name>.cma" =
  match derive ~describe:fixture ~watch_roots ~preloaded ~test_libs:[ "site_store" ] ~target:test_target () with
  | Ok t -> t.cmas = [ "examples/site/frontend/store/site_store.cma" ]
  | Error _ -> false

let%test "fallback: a framework dep the worker did NOT preload is refused" =
  (* drop "fennec" from the preload set — handlers transitively needs it ⇒ Needs_unpreloaded *)
  match
    derive ~describe:fixture ~watch_roots ~preloaded:[ "fennec.fur" ]
      ~test_libs:[ "site_handlers" ] ~target:test_target ()
  with
  | Error (Needs_unpreloaded "fennec") -> true
  | _ -> false

let%test "fallback: an undeclared test lib is reported" =
  match derive ~describe:fixture ~watch_roots ~preloaded ~test_libs:[ "nope_lib" ] ~target:test_target () with
  | Error (Unknown_lib "nope_lib") -> true
  | _ -> false

let%test "opam deps are NOT pre-emptively flagged (only project framework libs are checked)" =
  (* fmt (opam, local=false) is in fennec.fur's closure but absent from [preloaded]; it must NOT
     trigger Needs_unpreloaded — opam coverage is left to the worker's runtime Dynlink catch. *)
  match derive ~describe:fixture ~watch_roots ~preloaded:[ "fennec.fur"; "fennec" ]
          ~test_libs:[ "site_store" ] ~target:test_target ()
  with
  | Ok _ -> true
  | Error _ -> false

let%test "fallback: empty describe yields No_describe" =
  match derive ~describe:"()" ~watch_roots ~preloaded ~test_libs:[ "site_store" ] ~target:test_target () with
  | Error No_describe -> true
  | _ -> false

let%test "objects appends the cmo after the cmas" =
  match derive ~describe:fixture ~watch_roots ~preloaded ~test_libs:[ "site_store" ] ~target:test_target () with
  | Ok t -> objects t = [ "examples/site/frontend/store/site_store.cma"; t.cmo ]
  | Error _ -> false

let%test "cmo_of_target mangles dune__exe__ + capitalizes" =
  cmo_of_target "a/b/foo_test.exe" = "a/b/.foo_test.eobjs/byte/dune__exe__Foo_test.cmo"

let%test "a leaf-only test (no app libs) yields an empty cma list + just the cmo" =
  match derive ~describe:fixture ~watch_roots ~preloaded ~test_libs:[ "fennec.fur" ] ~target:test_target () with
  | Ok t -> t.cmas = [] && objects t = [ t.cmo ]
  | Error _ -> false
