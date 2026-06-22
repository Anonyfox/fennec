(* [fennec new NAME] — scaffold a minimal working frontend Fennec app. See new_app.mli.

   The emitted project is the smallest thing that (a) builds, (b) serves real SSR HTML, and (c) lights
   up the editor on its .mlx — proven by building + running this exact file set during development.
   It is SSR-only on purpose: a plain Paw route renders the .mlx component via Fennec.Fur.Handler, so
   the project needs no route_gen/client-bundle wiring and stays tiny. The files are produced as pure
   (path, contents) pairs so the shape is unit-tested with no disk IO; [run] writes them. *)

(* lowercase, collapse non-alphanumerics to '_', prefix a leading digit with '_' → a valid OCaml lib name *)
let lib_name_of (raw : string) : string =
  let b = Buffer.create (String.length raw) in
  String.iter
    (fun c ->
      let c = Char.lowercase_ascii c in
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then Buffer.add_char b c
      else Buffer.add_char b '_')
    raw;
  let s = Buffer.contents b in
  (* collapse runs of '_' and trim leading/trailing '_' for tidiness *)
  let s =
    let parts = String.split_on_char '_' s |> List.filter (fun p -> p <> "") in
    String.concat "_" parts
  in
  let s = if s = "" then "app" else s in
  if s.[0] >= '0' && s.[0] <= '9' then "_" ^ s else s

(* ── the file templates ──────────────────────────────────────────────────────────────────────────
   The dune-project text (with the dialect stanza + deps) is built inside [files] so it can interpolate
   the real project name; the rest are plain templates parameterized by the components-library name. *)

(* server.ml renders the .mlx component to HTML on GET / via Fennec.Fur.Handler. Note the DOUBLE call
   `(Hello.make ~name:"world" ()) ()`: a Fur component's [make] returns a render thunk (the trailing
   JSX), and applying it once more renders the vnode. *)
let server_ml =
  "(* The whole server: one endpoint, one route, rendering a .mlx component to HTML.\n\
  \   `fennec dev` discovers THIS file (it calls Fennec.serve) and runs it with livereload.\n\
  \   `Paw` is the HTTP toolkit — re-exported by fennec, so it's available directly. *)\n\n\
   (* GET / → render the Hello component. A Fur component's `make` returns a render thunk, so we\n\
  \   apply () a second time to produce the vnode Handler.html renders. *)\n\
   let home conn = Fennec.Fur.Handler.html conn ((Hello.make ~name:\"world\" ()) ())\n\n\
   let web =\n\
  \  Paw.endpoint ~name:\"web\" ~hosts:[ \"*\" ] ()\n\
  \  |> Paw.use (Paw.Logger.make ())\n\
  \  |> Paw.get \"/\" home\n\n\
   let () = Fennec.serve [ web ]\n"

(* console.ml — the entire `fennec console` entry: boot the app's runtime (no HTTP) into a REPL with
   the whole app + framework in scope. `Fennec.console_run` is not `Fennec.serve`, so server discovery
   ignores this target. *)
let console_ml = "let () = Fennec.console_run ()\n"

let server_dune ~comp_lib =
  Printf.sprintf
    "; The server executable. `fennec dev` builds server.bc (bytecode) for a fast loop; a release\n\
     ; build produces the native server.exe.\n\
     (executable\n\
    \ (name server)\n\
    \ (modes byte exe)\n\
    \ (modules server)\n\
    \ (libraries fennec fennec.fur fennec.fur.html fennec.web %s))\n\
     \n\
     ; `fennec console` — a REPL with the whole app + framework loaded, no HTTP. A bytecode toplevel:\n\
     ; the dev-only fennec.console.engine links the compiler, and -linkall lets the toplevel reach every\n\
     ; module. It calls Fennec.console_run (not Fennec.serve), so `fennec dev` discovery ignores it.\n\
     (executable\n\
    \ (name console)\n\
    \ (modes byte)\n\
    \ (modules console)\n\
    \ (link_flags (-linkall))\n\
    \ (libraries fennec fennec.console.engine fennec.fur fennec.fur.html fennec.web %s))\n\
     \n\
     ; `fennec dev` builds under a custom `fastdev` profile (= dev minus `-g`) for fast incremental\n\
     ; relinks. This (env) makes that profile behave: force js_of_ocaml SEPARATE compilation (dune only\n\
     ; auto-enables it for the built-in `dev` profile, so a custom one would fall back to slow\n\
     ; whole-program jsoo on every client edit) and drop source maps (no `-g`, so they'd map to nothing\n\
     ; and jsoo would warn). `fennec dev --debug` uses the standard `dev` profile instead.\n\
     (env\n\
    \ (fastdev\n\
    \  (ocamlc_flags\n\
    \   (:standard \\ -g))\n\
    \  (js_of_ocaml\n\
    \   (compilation_mode separate)\n\
    \   (flags\n\
    \    (:standard \\ --source-map-inline --source-map --sourcemap)))))\n"
    comp_lib comp_lib

let hello_mlx =
  (* a real .mlx: bare-text JSX children + {expr} interpolation (the fennec surface) + a colocated\n\
     [%%style] block (scoped automatically). Edit this and reload to see the dev loop. *)
  "(* Your first component. This is `.mlx` — JSX-identical: bare text children and {expr}\n\
  \   interpolation. The [%%style] block below is colocated and auto-scoped to this component. *)\n\
   [%%style {scss|\n\
   .hello { font-family: system-ui, -apple-system, sans-serif; text-align: center; padding: 3rem 1rem }\n\
   .hello-title { color: #c25b34; font-size: 2.2rem; margin: 0 }\n\
   .hello-sub { color: #555; margin-top: .5rem }\n\
   |scss}]\n\n\
   let make ~name () =\n\
  \  <main className=\"hello\">\n\
  \    <h1 className=\"hello-title\">Hello from Fennec, {name}!</h1>\n\
  \    <p className=\"hello-sub\">Edit frontend/hello.mlx and reload.</p>\n\
  \  </main>\n"

let frontend_dune ~comp_lib =
  Printf.sprintf
    "; The components library — every .mlx here is compiled with the Fur ppx and `-open Fur`, so\n\
     ; `h`/`text`/JSX resolve. Add more .mlx files freely; no dune change needed.\n\
     (library\n\
    \ (name %s)\n\
    \ (wrapped false)\n\
    \ (libraries fennec.fur fennec.fur.html)\n\
    \ (preprocess\n\
    \  (pps fennec.fur.ppx))\n\
    \ (flags\n\
    \  (:standard -w -a -open Fur)))\n"
    comp_lib

let gitignore = "_build/\n.fennec/\n*.install\n.merlin\n"

let readme ~name ~comp_lib:_ =
  Printf.sprintf
    "# %s\n\n\
     A minimal frontend Fennec app (server-rendered).\n\n\
     ## Run\n\n\
     ```sh\n\
     dune build      # compile (the .mlx is preprocessed by `fennec mlx-pp`)\n\
     fennec dev      # run with livereload — it prints the local URL to open\n\
     ```\n\n\
     ## Layout\n\n\
     - `server.ml` — the server: one endpoint, renders the component on `GET /`.\n\
     - `frontend/hello.mlx` — your first `.mlx` component (edit this).\n\
     - `dune-project` — declares the `.mlx` dialect + deps.\n\n\
     ## Editor not lighting up on `.mlx`?\n\n\
     Run `fennec doctor` — it checks the whole `.mlx` toolchain and prints the exact fix for\n\
     anything missing.\n\n\
     ## Adding interactivity (client-side)\n\n\
     This scaffold is server-rendered only. For client hydration / a live SPA (signals after load),\n\
     see the `examples/site` tree in the Fennec repo for the route_gen + client-bundle wiring.\n"
    name

(* assemble the file set for a project named [name] *)
let files ~(name : string) : (string * string) list =
  let comp_lib = lib_name_of name ^ "_components" in
  (* dune_project's lib-name interpolation goes through [_placeholder_name]; rebuild it directly here
     so it uses the REAL name (the top-level [dune_project] closes over the empty placeholder). *)
  let dune_project_text =
    Printf.sprintf
      "(lang dune 3.16)\n\n\
       ; The .mlx dialect: this stanza is what makes `.mlx` files compile (via `fennec mlx-pp`) and\n\
       ; light up in the editor (via the ocamlmerlin-fennec-mlx reader). Do not remove it. If `.mlx`\n\
       ; IntelliSense ever dies or the build can't find `fennec`, run `fennec doctor`.\n\
       %s\n\n\
       (package\n\
      \ (name %s)\n\
      \ (depends\n\
      \  (ocaml (>= 5.2))\n\
      \  (dune (>= 3.16))\n\
      \  ; the runtime framework (Fennec.serve, Fur, Paw)\n\
      \  fennec\n\
      \  ; the CLI + the .mlx dialect toolchain (the `fennec` binary IS `fennec mlx-pp`, plus the\n\
      \  ; ocamlmerlin-fennec-mlx editor reader). The mlx parser is VENDORED — no external `mlx` dep.\n\
      \  fennec-cli))\n"
      Doctor.stanza (lib_name_of name)
  in
  [ ("dune-project", dune_project_text);
    ("dune", server_dune ~comp_lib);
    ("server.ml", server_ml);
    ("console.ml", console_ml);
    ("frontend/dune", frontend_dune ~comp_lib);
    ("frontend/hello.mlx", hello_mlx);
    (".gitignore", gitignore);
    ("README.md", readme ~name ~comp_lib) ]

(* ── IO ──────────────────────────────────────────────────────────────────────────────────────── *)

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

(* a project name must be a sane directory name: no slashes / leading dot / control chars *)
let valid_project_name (s : string) : bool =
  s <> "" && s.[0] <> '.'
  && String.for_all (fun c -> c <> '/' && c <> '\\' && c <> '\000') s
  && not (String.contains s ' ')

let dir_is_empty dir =
  match Sys.readdir dir with [||] -> true | _ -> false | exception _ -> true

let run ?(in_dir = Sys.getcwd ()) (name : string) : int =
  if not (valid_project_name name) then begin
    Printf.eprintf
      "fennec new: invalid project name %S — use a directory-safe name (letters, digits, - or _)\n"
      name;
    1
  end
  else begin
    let target = Filename.concat in_dir name in
    if Sys.file_exists target && not (dir_is_empty target) then begin
      Printf.eprintf "fennec new: %s already exists and is not empty — pick another name or remove it\n" target;
      1
    end
    else begin
      let fs = files ~name in
      List.iter (fun (rel, contents) -> write_file (Filename.concat target rel) contents) fs;
      Printf.printf "Created %s/ — a minimal frontend Fennec app:\n" name;
      List.iter (fun (rel, _) -> Printf.printf "  %s/%s\n" name rel) fs;
      Printf.printf
        "\nNext:\n\
        \  cd %s\n\
        \  dune build              # compile (preprocesses the .mlx)\n\
        \  fennec dev              # run with livereload\n\
        \  fennec discover \"TASK\"   # find the Fennec API for whatever you build next\n\n\
         Stuck on the editor or build? Run `fennec doctor`.\n"
        name;
      flush stdout;
      0
    end
  end

(* ── tests (pure name sanitizer + file-set shape) ─────────────────────────────────────────────── *)

let%test "lib_name_of: dashes → underscores" = lib_name_of "my-cool-app" = "my_cool_app"
let%test "lib_name_of: uppercase → lowercase" = lib_name_of "MyApp" = "myapp"
let%test "lib_name_of: leading digit gets prefixed" = lib_name_of "3d" = "_3d"
let%test "lib_name_of: dots and spaces collapse" = lib_name_of "foo.bar baz" = "foo_bar_baz"
let%test "lib_name_of: empty → app" = lib_name_of "" = "app"
let%test "lib_name_of: collapses repeated separators" = lib_name_of "a--b__c" = "a_b_c"

let find_file name rel = List.assoc rel (files ~name)

let contains hay needle =
  let n = String.length needle and m = String.length hay in
  let rec go i = i + n <= m && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let%test "files: dune-project carries the dialect stanza" =
  contains (find_file "demo" "dune-project") "(merlin_reader fennec-mlx)"

let%test "files: dune-project declares fennec + fennec-cli, and NO external mlx dep" =
  let dp = find_file "demo" "dune-project" in
  contains dp "fennec-cli" && contains dp "fennec" && not (contains dp "(mlx ")

let%test "files: dune-project dialect runs `fennec mlx-pp` (the vendored in-process preprocessor)" =
  contains (find_file "demo" "dune-project") "(run %{bin:fennec} mlx-pp %{input-file})"

let%test "files: dune-project package name = the project name" =
  contains (find_file "my-app" "dune-project") "(name my_app)"

let%test "files: server.ml calls Fennec.serve and renders the component" =
  let s = find_file "demo" "server.ml" in
  contains s "Fennec.serve" && contains s "Hello.make" && contains s "Handler.html"

let%test "files: a starter .mlx component exists with bare-text JSX + {expr}" =
  let m = find_file "demo" "frontend/hello.mlx" in
  contains m "let make ~name ()" && contains m "{name}" && contains m "[%%style"

let%test "files: the frontend dune links the components under the derived lib name" =
  contains (find_file "my-app" "frontend/dune") "(name my_app_components)"

let%test "files: the server dune links that same components lib" =
  contains (find_file "my-app" "dune") "my_app_components"

let%test "files: the file set is exactly the expected paths" =
  let paths = List.map fst (files ~name:"demo") |> List.sort compare in
  paths = List.sort compare
    [ "dune-project"; "dune"; "server.ml"; "console.ml"; "frontend/dune"; "frontend/hello.mlx"; ".gitignore"; "README.md" ]
