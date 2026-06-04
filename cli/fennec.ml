(* The `fennec` command-line tool.

   Today it exposes a single subcommand, [build], which drives the native
   bundlers (esbuild for JavaScript, Lightning CSS + grass for CSS/SCSS) that are
   statically linked in via {!Fennec_buildkit}. One invocation handles a mix of
   JS and CSS entry points: the engine is chosen per input from its file
   extension, so callers never pick a tool by hand.

   A long-running [dev] subcommand runs the livereload dev loop: it discovers the
   server (the one executable calling [Fennec.serve], via {!Discover}), watches and
   supervises it, and hot-reloads the frontend. [build] is the one-shot production path. *)

let version = "0.0.1"

(* ---- small IO helpers (stdlib only) ---- *)

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let human_size n =
  if n < 1024 then Printf.sprintf "%d B" n
  else if n < 1024 * 1024 then Printf.sprintf "%.1f KB" (float_of_int n /. 1024.)
  else Printf.sprintf "%.1f MB" (float_of_int n /. 1024. /. 1024.)

let lower_ext path =
  match Filename.extension path with
  | "" -> ""
  | e -> String.lowercase_ascii (String.sub e 1 (String.length e - 1))

(* ---- the build command ---- *)

type kind = Js | Css | Scss | Unknown

let classify path =
  match lower_ext path with
  | "js" | "mjs" | "cjs" | "jsx" | "ts" | "tsx" -> Js
  | "css" -> Css
  | "scss" | "sass" -> Scss
  | _ -> Unknown

(* Build one JS bundle's bytes. In dev, `fennec dev` exports FENNEC_ESBUILD_WORKER —
   a warm esbuild worker holding the context across rebuilds — so delegate to it for
   a fast incremental rebuild, falling back to the cold one-shot on ANY failure (no
   worker, dead socket, timeout, build/internal error). The two paths are
   byte-identical: the worker builds from the same options JSON. *)
let build_js ~entry ~format ~global_name ~external_ ~minify ~sourcemap ~banner =
  let cold () =
    Fennec_buildkit.Esbuild.build ~entry ~format ~global_name ~external_ ~minify ~sourcemap ~banner
      ()
  in
  match Sys.getenv_opt "FENNEC_ESBUILD_WORKER" with
  | None | Some "" -> cold ()
  | Some socket ->
    let opts_json =
      Fennec_buildkit.Esbuild.json_opts ~entry ~format ~global_name ~external_ ~minify ~sourcemap
        ~banner
    in
    ( match Esbuild_worker.client_build ~socket ~opts_json with
    | Some bytes -> bytes
    | None -> cold () )

(* One entry point -> one output file in [outdir]. JS bundles to ".js", CSS/SCSS
   emit ".css", keeping the input's base name unless [out_name] overrides it.
   Returns (out_path, byte length). *)
let build_one ~outdir ~minify ~format ~global_name ~external_ ~sourcemap ~banner ~out_name input =
  if not (Sys.file_exists input) then failwith (Printf.sprintf "input not found: %s" input);
  let base = Filename.remove_extension (Filename.basename input) in
  let out_ext, output =
    match classify input with
    | Js ->
      (".js", build_js ~entry:input ~format ~global_name ~external_ ~minify ~sourcemap ~banner)
    | Css -> (".css", Fennec_buildkit.Css.transform ~minify (read_file input))
    (* path-aware: @use/@import resolve relative to the file, so a component's
       stylesheet can sit next to it and be pulled into an app's entry sheet *)
    | Scss -> (".css", Fennec_buildkit.Css.scss_path ~minify input)
    | Unknown ->
      failwith
        (Printf.sprintf "unsupported file type %S (expected .js/.ts/.jsx or .css/.scss)" input)
  in
  let filename = match out_name with Some n -> n | None -> base ^ out_ext in
  let out_path = Filename.concat outdir filename in
  mkdir_p (Filename.dirname out_path);
  write_file out_path output;
  (out_path, String.length output)

let run inputs outdir minify format global_name external_ sourcemap banner banner_file out_name
    public embed includes =
  match inputs with
  | [] when public = None && embed = None && includes = [] ->
    prerr_endline "fennec build: no input files given (try `fennec build --help`)";
    1
  | _ :: _ :: _ when out_name <> None ->
    prerr_endline "fennec build: --out-name requires exactly one input file";
    1
  | _ ->
    (try
       (* banner from --banner-file takes precedence over --banner *)
       let banner =
         match banner_file with
         | Some path ->
           if not (Sys.file_exists path) then
             failwith (Printf.sprintf "banner file not found: %s" path);
           read_file path
         | None -> banner
       in
       mkdir_p outdir;
       (* 1. build the bundle inputs into the output dir (the web root) *)
       List.iter
         (fun input ->
           let out_path, n =
             build_one ~outdir ~minify ~format ~global_name ~external_ ~sourcemap ~banner ~out_name
               input
           in
           Printf.printf "  %-28s -> %-32s %s\n" input out_path (human_size n))
         inputs;
       (* 2a. copy pre-built bundle files (from sibling dune rules) into the web
          root, clash-checked *)
       List.iter
         (fun spec ->
           match Webroot.include_file ~outdir ~spec with
           | Ok () -> Printf.printf "  include %-24s -> %s/\n" spec outdir
           | Error msg -> failwith msg)
         includes;
       (* 2b. stage public/ INTO the web root (after bundles, so clashes with a
          bundle output are caught and are hard errors) *)
       (match public with
       | Some dir -> (
         match Webroot.stage_public ~outdir ~public_dir:dir with
         | Ok () -> Printf.printf "  staged %-21s -> %s/\n" dir outdir
         | Error msg -> failwith msg)
       | None -> ());
       (* 3. optionally emit the prod embed module from the assembled web root *)
       (match embed with
       | Some file -> (
         match Webroot.emit_embed ~outdir ~out_file:file with
         | Ok n -> Printf.printf "  embedded %d file(s) -> %s\n" n file
         | Error msg -> failwith msg)
       | None -> ());
       flush stdout;
       0
     with Failure msg ->
       Printf.eprintf "fennec build: %s\n" msg;
       1)

(* ---- cmdliner wiring ---- *)

open Cmdliner

let inputs_arg =
  let doc =
    "Entry files to build. Each is processed independently and written to the output \
     directory. The engine is picked from the extension: .js/.mjs/.cjs/.jsx/.ts/.tsx are \
     bundled with esbuild; .css is optimized and .scss/.sass are compiled with Lightning CSS \
     + grass."
  in
  (* may be empty when only --public/--embed are used (staging-only rule) *)
  Arg.(value & pos_all string [] & info [] ~docv:"INPUT" ~doc)

let outdir_arg =
  let doc = "Output directory (created if missing)." in
  Arg.(value & opt string "dist" & info [ "o"; "outdir" ] ~docv:"DIR" ~doc)

let no_minify_arg =
  let doc = "Disable minification. Production builds minify by default." in
  Arg.(value & flag & info [ "no-minify" ] ~doc)

let format_arg =
  let doc = "JavaScript output format: $(b,esm), $(b,iife), or $(b,cjs). Ignored for CSS." in
  Arg.(value & opt string "esm" & info [ "format" ] ~docv:"FMT" ~doc)

let global_name_arg =
  let doc =
    "Global variable name for the bundle's exports. Only meaningful with $(b,--format iife)."
  in
  Arg.(value & opt string "" & info [ "global-name" ] ~docv:"NAME" ~doc)

let external_arg =
  let doc =
    "Mark an import path as external (left unbundled). Repeatable, e.g. \
     $(b,--external react --external react-dom)."
  in
  Arg.(value & opt_all string [] & info [ "external" ] ~docv:"MODULE" ~doc)

let sourcemap_arg =
  let doc = "Emit an inline source map (JavaScript only)." in
  Arg.(value & flag & info [ "sourcemap" ] ~doc)

let banner_arg =
  let doc = "Text prepended verbatim to each JavaScript bundle (e.g. a license header)." in
  Arg.(value & opt string "" & info [ "banner" ] ~docv:"TEXT" ~doc)

let banner_file_arg =
  let doc =
    "Read the JavaScript banner from a file (e.g. a require-shim). Takes precedence over \
     $(b,--banner)."
  in
  Arg.(value & opt (some string) None & info [ "banner-file" ] ~docv:"FILE" ~doc)

let out_name_arg =
  let doc =
    "Output filename (within the output directory), instead of deriving it from the input. \
     Requires exactly one input."
  in
  Arg.(value & opt (some string) None & info [ "out-name" ] ~docv:"NAME" ~doc)

let public_arg =
  let doc =
    "Stage a static directory (e.g. $(b,public)) into the output directory, paths preserved, \
     AFTER the bundle inputs. A file that collides with a build output at the same path is a \
     hard error. The output directory becomes the web root (bundles + static, served together)."
  in
  Arg.(value & opt (some string) None & info [ "public" ] ~docv:"DIR" ~doc)

let embed_arg =
  let doc =
    "Emit an OCaml module ($(i,FILE)) mapping each file in the assembled output directory to its \
     bytes, for compile-time embedding into the binary (prod: a single self-contained \
     executable). Exposes $(b,lookup : string -> string option) and $(b,paths : string list)."
  in
  Arg.(value & opt (some string) None & info [ "embed" ] ~docv:"FILE" ~doc)

let include_arg =
  let doc =
    "Copy a pre-built file (a bundle produced by a sibling rule) into the web root at its \
     basename. Repeatable. A clash with another web-root file is a hard error."
  in
  Arg.(value & opt_all string [] & info [ "include" ] ~docv:"FILE" ~doc)

let build_term =
  let go inputs outdir no_minify format global_name external_ sourcemap banner banner_file out_name
      public embed includes =
    run inputs outdir (not no_minify) format global_name external_ sourcemap banner banner_file
      out_name public embed includes
  in
  Term.(
    const go $ inputs_arg $ outdir_arg $ no_minify_arg $ format_arg $ global_name_arg
    $ external_arg $ sourcemap_arg $ banner_arg $ banner_file_arg $ out_name_arg $ public_arg
    $ embed_arg $ include_arg)

let build_cmd =
  let doc = "Build JavaScript and CSS for production" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Bundle and optimize JavaScript and CSS in a single pass. Every input is routed to the \
         right native engine by its file extension, so one command covers a whole project's \
         assets. All production optimizations (minification, tree-shaking, dead-code \
         elimination, CSS nesting/calc reduction) are on by default; the flags below tune or \
         disable them.";
      `S Manpage.s_examples;
      `P "Build an app's JS entry and stylesheet into ./dist:";
      `Pre "  fennec build src/main.ts styles/app.scss";
      `P "Unminified ESM with a source map into ./build:";
      `Pre "  fennec build -o build --no-minify --sourcemap src/main.ts";
      `P "An IIFE bundle exposing a global, leaving React external:";
      `Pre "  fennec build --format iife --global-name App --external react src/widget.jsx";
      `S "WEB ROOT";
      `P
        "The output directory ($(b,-o)) is the web root: bundle outputs and the staged \
         $(b,--public) tree live there together and are served at their paths. Each bundle is its \
         own $(b,build) invocation (a dune rule) with its own flags and $(b,--out-name) subpath — \
         dune is the bundle manifest. $(b,--public) stages a static tree in afterward (a clash \
         with a bundle output is fatal); $(b,--embed) bakes the assembled root into an OCaml \
         module for a single self-contained prod binary.";
      `P "Stage a static tree into the web root (dev):";
      `Pre "  fennec build src/main.ts --out-name app.js -o webroot";
      `Pre "  fennec build --public public -o webroot";
      `P "Same, plus embed the assembled root for prod:";
      `Pre "  fennec build --public public --embed webroot_assets.ml -o webroot" ]
  in
  Cmd.v (Cmd.info "build" ~doc ~man) build_term

let dev_cmd =
  let target_arg =
    let doc = "Override the dune target(s) to build and watch (default: derived from the server)." in
    Arg.(value & opt (some string) None & info [ "target" ] ~docv:"TARGET" ~doc)
  in
  let exe_arg =
    let doc =
      "Path to the built server executable to run (under _build). Optional: with no path, the \
       server is discovered as the one executable that calls $(b,Fennec.serve)."
    in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"SERVER_EXE" ~doc)
  in
  let assets_arg =
    let doc =
      "Name of the served web-root directory (a subdir of the server exe's build dir) whose \
       bundles drive frontend livereload. Default: $(b,webroot)."
    in
    Arg.(value & opt string "webroot" & info [ "assets" ] ~docv:"DIR" ~doc)
  in
  let dry_arg =
    let doc = "Print the discovered server (target/exe) and exit, without running." in
    Arg.(value & flag & info [ "dry-run"; "print" ] ~doc)
  in
  let go target exe assets dry =
    (* what dune watches: an explicit --target if given, else the discovered server bytecode PLUS
       the served web-root dir (so the client bundle rebuilds too, not just the SSR server) *)
    let dev_targets (d : Discover.t) =
      match target with Some t -> [ t ] | None -> d.Discover.targets @ [ Filename.concat d.Discover.src_dir assets ]
    in
    if dry then (
      match Discover.find () with
      | Ok d ->
        Printf.printf "server:  %s\nroot:    %s\ntargets: %s\nexe:     %s\n" d.Discover.name d.Discover.root
          (String.concat " " (dev_targets d))
          d.Discover.exe;
        0
      | Error msg ->
        Printf.eprintf "fennec dev: %s\n" msg;
        1)
    else begin
      (* GUARANTEE a clean slate before this dev session — so no leftover from a previous run
         (even a SIGKILL'd one) can hold the dev port. Two layered defences, run BEFORE
         discovery's `dune describe`:
           1. `dune shutdown` — stop any orphaned `dune build --watch` for this workspace (its
              own RPC), so the new build isn't forwarded to a wedged daemon serving stale files;
           2. reap the recorded previous child tree from the pidfile (by pid AND identity, so a
              recycled pid is never killed) — this frees the port directly, and the previous
              supervisor's server also self-exits the moment it sees it's been reparented.
         We deliberately do NOT scan the process table by name (e.g. `pgrep -f 'fennec dev'`):
         a command-line substring match would SIGKILL unrelated processes like
         `vim fennec dev notes.txt`, and the pidfile already covers the legitimate case. *)
      ignore (Sys.command "dune shutdown >/dev/null 2>&1");
      Fennec_dev.Pidfile.reap_stale ~cwd:(Sys.getcwd ());
      match exe with
      | Some exe_path ->
        (* Supervisor.run blocks until killed; the 0 is unreachable, only there for the type *)
        Fennec_dev.Supervisor.run ~targets:(match target with Some t -> [ t ] | None -> [ "@@default" ]) ~exe:exe_path ~assets;
        0
      | None -> (
        match Discover.find () with
        | Error msg ->
          Printf.eprintf "fennec dev: %s\n" msg;
          1
        | Ok d ->
          (* Supervisor expects to run from the workspace root; discovery already scoped to the cwd *)
          Sys.chdir d.Discover.root;
          Fennec_dev.Supervisor.run ~targets:(dev_targets d) ~exe:d.Discover.exe ~assets;
          0)
    end
  in
  let doc = "Run the dev server with livereload" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Start a development loop: run $(b,dune build --watch) (dune is the sole source watcher \
         and builder — including assets, which are dune rules that call $(b,fennec build)) and \
         supervise the server executable. The CLI watches the build OUTPUT with a native \
         filesystem-event watcher: a backend change rebuilds the exe and the server is \
         restarted; a frontend-only edit live-reloads without a restart, the CLI signalling the \
         server's dev control socket to hot-swap CSS or reload.";
      `P
        "With no $(i,SERVER_EXE), the server is found by asking dune (via $(b,dune describe)) for \
         the one executable whose source calls $(b,Fennec.serve) — so $(b,fennec dev) just works \
         from a project (or app sub-)directory, with nothing to configure. Zero or more than one \
         such executable is a clean error.";
      `P
        "This command is pure convenience. The project is a plain dune project: $(b,dune build \
         --watch) plus $(b,dune exec) still builds and runs it — you lose only the automated \
         restart and CSS hot-swap.";
      `S Manpage.s_examples;
      `Pre "  fennec dev                 # discover the server and run it";
      `Pre "  fennec dev --dry-run       # show what would run";
      `Pre "  fennec dev --target @examples/site/dev _build/default/examples/site/server.bc" ]
  in
  Cmd.v (Cmd.info "dev" ~doc ~man) Term.(const go $ target_arg $ exe_arg $ assets_arg $ dry_arg)

(* Internal: the persistent esbuild worker `fennec dev` spawns. Not for direct use. *)
let worker_cmd =
  let socket_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"SOCKET") in
  let go socket = Esbuild_worker.serve ~socket (* loops until signalled; never returns *) in
  let doc = "(internal) persistent esbuild build worker used by fennec dev" in
  Cmd.v (Cmd.info "__esbuild-worker" ~doc) Term.(const go $ socket_arg)

let main_cmd =
  let doc = "Fennec — native JavaScript & CSS build tooling" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Fennec bundles JavaScript (esbuild) and compiles/optimizes CSS and SCSS (Lightning CSS \
         + grass) from a single self-contained binary — no Node, no separate toolchain. It also \
         drives the development lifecycle (see the $(b,dev) command).";
      `S Manpage.s_commands ]
  in
  let info = Cmd.info "fennec" ~version ~doc ~man in
  Cmd.group info [ build_cmd; dev_cmd; worker_cmd ]

let () = exit (Cmd.eval' main_cmd)
