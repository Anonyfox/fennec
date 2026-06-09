(* The `fennec` command-line tool: a native asset bundler and the framework's dev + test CLI,
   sitting beside dune (which owns the build graph and is the sole watcher of source).

   Three user commands:
   - [build] bundles JS (esbuild) and CSS/SCSS (Lightning CSS + grass), statically linked via
     {!Fennec_buildkit} — one self-contained binary, engine chosen per input extension. It's a
     general-purpose bundler; in a Fennec project, dune rules call it.
   - [dev] runs the supervised livereload loop: discover the server (the executable calling
     [Fennec.serve], via {!Discover}), watch the build OUTPUTS, restart on a backend change, and
     hot-reload the frontend without a restart.
   - [test] runs and verifies the app — unit/http/browser/system tests plus doc-coverage
     ([fennec test docs]); delegates to {!Fennec_testcmd}.

   Plus plumbing not run by hand (listed under INTERNAL COMMANDS): [__esbuild-worker], the warm
   worker `fennec dev` spawns, and [gen-doctests], codegen a dune rule calls so .mli examples run
   under `fennec test`. *)

let version = "0.0.1"

module Discover = Fennec_dev.Discover (* server discovery now lives in the (tested) dev library *)

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

let rec project_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then Sys.getcwd () else project_root parent

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
  let clean_arg =
    let doc =
      "Run $(b,dune clean) before starting — a full rebuild from scratch. Off by default; use it \
       only to recover from a corrupt local $(b,_build) that a normal restart doesn't fix. For a \
       normal fennec app (deps installed via opam) this rebuilds only your app; in a workspace \
       with vendored dependencies it rebuilds those too, which is slow."
    in
    Arg.(value & flag & info [ "clean" ] ~doc)
  in
  let port_arg =
    let doc =
      "Base port for the dev server (default 8020). The host-routed gateway listens here and each \
       non-default endpoint gets the next port up. A distinct $(b,--port) runs a fully isolated \
       instance — useful for parallel worktrees/agents — since it shifts the whole port block."
    in
    Arg.(value & opt (some int) None & info [ "port" ] ~docv:"PORT" ~doc)
  in
  let mongo_arg =
    let doc =
      "Launch a managed MongoDB ($(b,mongod)) for the dev session and point the app at it via \
       $(b,MONGO_URL), so the data layer uses the real driver instead of the in-memory engine. The \
       instance is ephemeral, isolated, and reaped on exit (no dangling process); an absent mongod \
       degrades to in-memory."
    in
    Arg.(value & flag & info [ "mongo" ] ~doc)
  in
  let agent_arg =
    let doc =
      "Emit a machine-readable agent event journal alongside the human dev UI. Agents can then \
       configure $(b,fennec agent hook --timeout 12) as a post-tool hook instead of parsing \
       terminal output or running build/test probes after each edit."
    in
    Arg.(value & flag & info [ "agent" ] ~doc)
  in
  let attach_arg =
    let doc =
      "With $(b,--agent), try to attach the current coding harness dynamically by installing one \
       guarded post-tool hook that runs $(b,fennec agent hook --timeout 12). This writes \
       user-level harness config, never repo files."
    in
    Arg.(value & flag & info [ "attach" ] ~doc)
  in
  let agent_dir_arg =
    let doc =
      "Directory for the agent event journal. Implies $(b,--agent). Default uses \
       $(b,FENNEC_AGENT_DIR), then XDG state, scoped by project root."
    in
    Arg.(value & opt (some string) None & info [ "agent-dir" ] ~docv:"DIR" ~doc)
  in
  let go target exe assets dry clean port mongo agent attach agent_dir =
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
      (* opt-in nuclear heal: a full `dune clean` (after the daemon is stopped) for the rare case
         where _build is corrupt in a way a normal restart can't fix. The next build is from
         scratch — fast for an opam-installed fennec (only your app rebuilds), slow with vendored
         deps. Announced because it can pause a while. *)
      if clean then (Printf.printf "fennec dev: dune clean (full rebuild)…\n%!"; ignore (Sys.command "dune clean >/dev/null 2>&1"));
      (* --mongo: a managed single-node replica-set mongod for the dev session (a replica set so
         change streams work); MONGO_URL points the app (spawned below by the supervisor) at it. The
         lifecycle's at_exit reaps it when dev exits (Ctrl-C → graceful shutdown → exit → stop +
         clean the ephemeral data dir); an absent mongod degrades to the in-memory backend. *)
      if mongo then ignore (Fennec_dev.Mongo_rs.launch ());
      let agent = agent || attach in
      let attach_agent ~root agent_dir =
        if attach then (
          let results = Fennec_dev.Agent_attach.install ~root ~agent_dir () in
          Printf.printf "%s\n%!" (Fennec_dev.Agent_attach.report results))
      in
      match exe with
      | Some exe_path ->
        let root = project_root (Sys.getcwd ()) in
        let agent_dir =
          match agent_dir with
          | Some d -> Some d
          | None -> if agent then Some (Fennec_dev.Agent_event.default_dir ~root) else None
        in
        (match agent_dir with Some dir -> attach_agent ~root dir | None -> ());
        (* explicit-exe override (multi-server repos): we don't run discovery, so we don't know the
           server's precise build targets. With no --target we watch [@@default] — broader than the
           discovered path's scoped [server.bc + webroot], so each edit rebuilds more than strictly
           needed, but it's CORRECT: @@default includes the web root, so frontend livereload still
           works. Pass --target to scope it. (Supervisor.run blocks until killed; 0 is for the type.) *)
        Fennec_dev.Supervisor.run ?port ?agent_dir ~targets:(match target with Some t -> [ t ] | None -> [ "@@default" ]) ~exe:exe_path ~assets;
        0
      | None -> (
        match Discover.find () with
        | Error msg ->
          Printf.eprintf "fennec dev: %s\n" msg;
          1
        | Ok d ->
          (* Supervisor expects to run from the workspace root; discovery already scoped to the cwd *)
          Sys.chdir d.Discover.root;
          let agent_dir =
            match agent_dir with Some d -> Some d | None -> if agent then Some (Fennec_dev.Agent_event.default_dir ~root:d.Discover.root) else None
          in
          (match agent_dir with Some dir -> attach_agent ~root:d.Discover.root dir | None -> ());
          Fennec_dev.Supervisor.run ?port ?agent_dir ~targets:(dev_targets d) ~exe:d.Discover.exe ~assets;
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
      `Pre "  fennec dev --agent         # same, plus an agent event journal";
      `Pre "  fennec dev --agent --attach # same, and attach the current coding harness";
      `Pre "  fennec dev --dry-run       # show what would run";
      `Pre "  fennec dev --port 9000     # run an isolated instance on a different port block";
      `Pre "  fennec dev --target @examples/site/dev _build/default/examples/site/server.bc" ]
  in
  Cmd.v (Cmd.info "dev" ~doc ~man)
    Term.(const go $ target_arg $ exe_arg $ assets_arg $ dry_arg $ clean_arg $ port_arg $ mongo_arg $ agent_arg $ attach_arg $ agent_dir_arg)

let agent_cmd =
  let dir_arg =
    let doc = "Agent state directory. Default uses FENNEC_AGENT_DIR, then XDG state, scoped by project root." in
    Arg.(value & opt (some string) None & info [ "dir" ] ~docv:"DIR" ~doc)
  in
  let timeout_arg =
    let doc = "Maximum seconds to wait for the next dev event." in
    Arg.(value & opt float 12.0 & info [ "timeout" ] ~docv:"SECONDS" ~doc)
  in
  let after_arg =
    let doc = "Wait only for an event whose id is greater than $(docv)." in
    Arg.(value & opt (some int) None & info [ "after" ] ~docv:"ID" ~doc)
  in
  let state_dir dir =
    match dir with
    | Some d -> d
    | None -> Fennec_dev.Agent_event.default_dir ~root:(project_root (Sys.getcwd ()))
  in
  let status =
    let go dir =
      print_string (Fennec_dev.Agent_event.status ~dir:(state_dir dir));
      0
    in
    Cmd.v (Cmd.info "status" ~doc:"Print the current agent attachment state") Term.(const go $ dir_arg)
  in
  let wait =
    let go dir timeout after =
      match Fennec_dev.Agent_event.wait_next ?after ~dir:(state_dir dir) ~timeout () with
      | Ok (_id, s) -> print_endline s; 0
      | Error msg -> prerr_endline msg; 1
    in
    Cmd.v (Cmd.info "wait" ~doc:"Low-level: wait for the next fennec dev agent event") Term.(const go $ dir_arg $ timeout_arg $ after_arg)
  in
  let mark =
    let go dir =
      let input = try In_channel.input_all stdin with _ -> "" in
      let id = Fennec_dev.Agent_event.mark ~dir:(state_dir dir) ~input in
      Printf.printf "%d\n" id;
      0
    in
    Cmd.v (Cmd.info "mark" ~doc:"Low-level: snapshot the latest event id for hook implementations") Term.(const go $ dir_arg)
  in
  let hook =
    let go dir timeout =
      let input = try In_channel.input_all stdin with _ -> "" in
      let event =
        try
          let name =
            match Fennec_dev.Agent_event.find_string_field input "hook_event_name" with
            | Some _ as v -> v
            | None -> Fennec_dev.Agent_event.find_string_field input "hookEventName"
          in
          match name with
          | Some s -> Fennec_dev.Agent_event.unescape_json_string s
          | None -> "PostToolUse"
        with _ -> "PostToolUse"
      in
      print_endline (Fennec_dev.Agent_event.hook_json ~dir:(state_dir dir) ~timeout ~event ~input);
      0
    in
    Cmd.v (Cmd.info "hook" ~doc:"Post-tool hook command: emit hookSpecificOutput.additionalContext JSON") Term.(const go $ dir_arg $ timeout_arg)
  in
  let doc = "Agent-facing hook helpers for fennec dev --agent" in
  Cmd.group (Cmd.info "agent" ~doc) [ status; mark; wait; hook ]

let skill_cmd =
  let go () =
    print_string (Fennec_dev.Skill_doc.render ());
    0
  in
  let doc = "Print generated Fennec guide content for humans and coding agents" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Print a terse Markdown guide that doubles as SKILL.md-style content for coding agents. \
         It is generated by the installed $(b,fennec) binary; no harness files are installed or \
         written." ]
  in
  Cmd.v (Cmd.info "skill" ~doc ~man) Term.(const go $ const ())

(* Internal: the persistent esbuild worker `fennec dev` spawns. Not for direct use. *)
let worker_cmd =
  let socket_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"SOCKET") in
  let go socket = Esbuild_worker.serve ~socket (* loops until signalled; never returns *) in
  let doc = "(internal) persistent esbuild build worker used by fennec dev" in
  Cmd.v (Cmd.info "__esbuild-worker" ~doc ~docs:"INTERNAL COMMANDS") Term.(const go $ socket_arg)

(* Run + verify the app across five cuts. The default (no SUITE) is the fast unit gate;
   $(b,http) and $(b,browser) each boot a dedicated, isolated app instance per suite (its own
   port — and, later, its own database) so stateful suites run in parallel deterministically;
   $(b,system) drives the real $(b,fennec dev) lifecycle (replacing the old e2e/*.sh scripts);
   $(b,docs) checks doc-coverage (warn by default). $(b,new <cut> <name>) scaffolds a suite. *)
let test_cmd =
  let module R = Fennec_testcmd.Run in
  let pos_arg =
    Arg.(value & pos_all string [] & info [] ~docv:"SUITE"
           ~doc:"Which cut: unit (default), http, browser, system, docs, all — $(b,docs PATH...) checks doc-coverage; $(b,new <cut> <name>) scaffolds a suite.")
  in
  let grep_arg = Arg.(value & opt (some string) None & info [ "grep"; "g" ] ~docv:"RE" ~doc:"Run only tests whose name (the $(b,let%http)/$(b,let%browser)/$(b,let%system) label) contains $(docv). A filter that matches nothing fails — never a silent pass.") in
  let max_failures_arg = Arg.(value & opt (some int) None & info [ "max-failures"; "x" ] ~docv:"N" ~doc:"Stop after $(docv) suites fail.") in
  let no_fail_fast_arg = Arg.(value & flag & info [ "no-fail-fast" ] ~doc:"Run every suite even after a failure (default stops at the first).") in
  let reporter_arg = Arg.(value & opt (some string) None & info [ "reporter" ] ~docv:"R" ~doc:"Browser cut: reporter style ($(b,auto), $(b,plain), $(b,pretty)).") in
  let jobs_arg = Arg.(value & opt (some int) None & info [ "jobs"; "j" ] ~docv:"N" ~doc:"Parallel suites (default: CPUs; $(b,-j1) forces serial).") in
  let headed_arg = Arg.(value & flag & info [ "headed" ] ~doc:"Browser cut: show the browser window.") in
  let screenshots_arg = Arg.(value & opt (some string) None & info [ "screenshots" ] ~docv:"DIR" ~doc:"Browser cut: write a PNG on failure into $(docv).") in
  let port_arg = Arg.(value & opt int R.default_options.base_port & info [ "port" ] ~docv:"BASE" ~doc:"Base port for per-suite instance blocks.") in
  let strict_arg = Arg.(value & flag & info [ "strict" ] ~doc:"Docs cut: fail (exit non-zero) on any undocumented or $(b,.ml)-only export — a CI gate. Default: warn only.") in
  let private_arg = Arg.(value & flag & info [ "private" ] ~doc:"Docs cut: also check $(b,.ml) top-level definitions, not just $(b,.mli) exports.") in
  let promote_arg = Arg.(value & flag & info [ "promote" ] ~doc:"Docs cut: move each doc that lives only in a $(b,.ml) up into the sibling $(b,.mli), where it renders. Idempotent; the $(b,.mli) wins on conflict.") in
  let mongo_arg = Arg.(value & flag & info [ "mongo" ] ~doc:"Launch a managed MongoDB ($(b,mongod)) for the run and point the app at it via $(b,MONGO_URL), so the data layer uses the real driver instead of the in-memory engine. The instance is ephemeral, isolated, and torn down on exit (no dangling process); an absent mongod degrades to in-memory.") in
  let go positionals grep max_failures no_fail_fast reporter jobs headed screenshots base_port strict private_ promote mongo =
    let opts ?(paths = []) suite =
      { R.suite; grep; max_failures; fail_fast = not no_fail_fast; reporter; jobs; headed;
        screenshots; base_port; mongo; strict; private_; promote; paths }
    in
    match positionals with
    | "new" :: rest -> R.scaffold rest               (* fennec test new <cut> <name> — scaffold a suite *)
    | "docs" :: paths -> R.run (opts ~paths R.Docs)  (* trailing positionals are paths to check *)
    | [] -> R.run (opts R.Unit)
    | suite :: _ ->
      (match R.suite_of_string suite with
       | Error msg -> Printf.eprintf "fennec test: %s\n" msg; 1
       | Ok suite -> R.run (opts suite))
  in
  let doc = "Run + verify the app: tests (unit, http, browser, system) and doc-coverage (docs)" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Run the app's tests in one of five cuts. $(b,fennec test) with no argument runs the \
         fast $(b,unit) gate (delegates to $(b,dune runtest)). $(b,http) and $(b,browser) boot a \
         DEDICATED, isolated app instance per suite — its own port (and, in future, its own \
         database) — so stateful suites run in parallel, deterministically, without sharing \
         state. $(b,system) drives the real $(b,fennec dev) lifecycle end-to-end (process \
         hygiene, port reclaim, host routing, livereload, the error panel) — the typed, \
         deterministic replacement for the old $(b,e2e/*.sh) scripts. $(b,docs) checks \
         doc-coverage — a verification like any other, but WARN by default ($(b,--strict) makes \
         it a CI gate; $(b,--promote) moves $(b,.ml)-only docs into the $(b,.mli)). $(b,all) runs \
         unit, then http, then browser, then system, then docs (fast-to-slow; docs only warns, so \
         $(b,all) stays green on a half-documented tree unless $(b,--strict)).";
      `P
        "Suites live by convention in $(b,test/http/), $(b,test/browser/), and $(b,test/system/). \
         Authoring is zero-ceremony: drop a $(b,*_test.ml) with a $(b,let%http) / $(b,let%browser) / \
         $(b,let%system) block — no main, no env wiring, no dune edit (the cut's library picks new \
         files up). $(b,fennec test new <cut> <name>) scaffolds the first one. For http/browser \
         fennec owns the lifecycle — build, boot a per-suite isolated instance, run, tear down. \
         System suites instead spawn $(b,fennec dev) themselves (the System layer reaps the whole \
         process group on teardown). A system scenario written with $(b,let%system_manual) is built \
         but skipped unless $(b,--manual) (e.g. one that runs $(b,fennec dev --clean), which wipes \
         the shared _build).";
      `S Manpage.s_examples;
      `Pre "  fennec test                    # the fast unit gate";
      `Pre "  fennec test http               # the Http suites, each isolated";
      `Pre "  fennec test browser -j1        # the Browser suites, serially";
      `Pre "  fennec test system             # drive the real fennec dev (was e2e/*.sh)";
      `Pre "  fennec test all                # everything, fast-to-slow";
      `Pre "  fennec test docs               # doc-coverage (warn-only)";
      `Pre "  fennec test docs --strict      # …make missing docs a CI gate";
      `Pre "  fennec test docs --promote     # move .ml-only docs into the .mli";
      `Pre "  fennec test new system reclaim # scaffold test/system/reclaim_test.ml" ]
  in
  Cmd.v (Cmd.info "test" ~doc ~man)
    Term.(const go $ pos_arg $ grep_arg $ max_failures_arg $ no_fail_fast_arg $ reporter_arg $ jobs_arg $ headed_arg $ screenshots_arg $ port_arg $ strict_arg $ private_arg $ promote_arg $ mongo_arg)

(* Generate (to stdout) a module running the executable {@ocaml[ ]} doc examples from the .mli
   interfaces in a directory. Not run by hand — wired by a one-time dune (rule), the route_gen
   pattern, so an example in a public interface both renders in odoc AND runs as a test. *)
let gen_doctests_cmd =
  let dir_arg = Arg.(value & pos 0 string "." & info [] ~docv:"DIR" ~doc:"Directory whose .mli interfaces to scan (default: the current directory).") in
  let go dir = print_string (Fennec_docs.Doctest_gen.generate ~dir); 0 in
  let doc = "(internal) generate .mli doctests for a dune rule" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Emit, to stdout, an OCaml module that runs every executable $(b,{@ocaml[ ... ]}) example \
         in the $(b,.mli) interfaces under $(docv) as a test. Intended for a one-time dune rule \
         (like $(b,route_gen)) so interface examples render in odoc AND execute:";
      `Pre "  (rule (deps (glob_files *.mli))";
      `Pre "        (action (with-stdout-to fennec_doctests.ml (run %{bin:fennec} gen-doctests .))))";
      `P "The generated module joins the library via $(b,\\(modules :standard\\)) and runs under $(b,fennec test)." ]
  in
  Cmd.v (Cmd.info "gen-doctests" ~doc ~man ~docs:"INTERNAL COMMANDS") Term.(const go $ dir_arg)

let main_cmd =
  let doc = "Native asset bundler and the Fennec framework's dev + test CLI" in
  let man =
    [ `S Manpage.s_description;
      `P
        "$(b,fennec) is the command-line companion to a Fennec app's $(b,dune) build. dune owns \
         the build graph and is the sole watcher of your source; $(b,fennec) does the parts dune \
         can't: it bundles assets — JavaScript via esbuild, CSS/SCSS via Lightning CSS + grass, \
         from one self-contained binary (no Node, no separate toolchain) — and it drives the \
         development and verification lifecycle.";
      `P
        "$(b,build) bundles assets (and is what your dune rules call); $(b,dev) runs the \
         supervised livereload loop; $(b,test) runs and verifies everything — unit, http, \
         browser, and system tests, plus doc-coverage ($(b,fennec test docs)).";
      `P
        "$(b,fennec) is convenience and quality on top, never a replacement for dune: delete it \
         and the project is still a plain dune project — $(b,dune build) and $(b,dune exec) build \
         and run it. You lose only the automated restart, CSS hot-swap, and orchestrated test cuts.";
      `S Manpage.s_commands;
      `S "INTERNAL COMMANDS";
      `P "Invoked by dune rules or by $(b,fennec dev) itself — not run by hand." ]
  in
  let info = Cmd.info "fennec" ~version ~doc ~man in
  let default =
    Term.(const (fun () -> print_string (Fennec_dev.Skill_doc.render ()); 0) $ const ())
  in
  Cmd.group info ~default [ build_cmd; dev_cmd; agent_cmd; skill_cmd; test_cmd; gen_doctests_cmd; worker_cmd ]

let () = exit (Cmd.eval' main_cmd)
