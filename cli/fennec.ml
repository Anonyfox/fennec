(* The `fennec` command-line tool.

   Today it exposes a single subcommand, [build], which drives the native
   bundlers (esbuild for JavaScript, Lightning CSS + grass for CSS/SCSS) that are
   statically linked in via {!Fennec_buildkit}. One invocation handles a mix of
   JS and CSS entry points: the engine is chosen per input from its file
   extension, so callers never pick a tool by hand.

   A long-running [dev] subcommand (a livereload dev server holding a warm build
   context) will be added later; [build] is the one-shot production path. *)

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

(* One entry point -> one output file in [outdir]. JS bundles to ".js", CSS/SCSS
   emit ".css", keeping the input's base name unless [out_name] overrides it.
   Returns (out_path, byte length). *)
let build_one ~outdir ~minify ~format ~global_name ~external_ ~sourcemap ~banner ~out_name input =
  if not (Sys.file_exists input) then failwith (Printf.sprintf "input not found: %s" input);
  let base = Filename.remove_extension (Filename.basename input) in
  let out_ext, output =
    match classify input with
    | Js ->
      ( ".js",
        Fennec_buildkit.Esbuild.build ~entry:input ~format ~global_name ~external_ ~minify
          ~sourcemap ~banner () )
    | Css -> (".css", Fennec_buildkit.Css.transform ~minify (read_file input))
    | Scss -> (".css", Fennec_buildkit.Css.scss ~minify (read_file input))
    | Unknown ->
      failwith
        (Printf.sprintf "unsupported file type %S (expected .js/.ts/.jsx or .css/.scss)" input)
  in
  let filename = match out_name with Some n -> n | None -> base ^ out_ext in
  let out_path = Filename.concat outdir filename in
  mkdir_p (Filename.dirname out_path);
  write_file out_path output;
  (out_path, String.length output)

let run inputs outdir minify format global_name external_ sourcemap banner banner_file out_name =
  match inputs with
  | [] ->
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
       List.iter
         (fun input ->
           let out_path, n =
             build_one ~outdir ~minify ~format ~global_name ~external_ ~sourcemap ~banner ~out_name
               input
           in
           Printf.printf "  %-28s -> %-32s %s\n" input out_path (human_size n))
         inputs;
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
  Arg.(non_empty & pos_all string [] & info [] ~docv:"INPUT" ~doc)

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

let build_term =
  let go inputs outdir no_minify format global_name external_ sourcemap banner banner_file out_name =
    run inputs outdir (not no_minify) format global_name external_ sourcemap banner banner_file
      out_name
  in
  Term.(
    const go $ inputs_arg $ outdir_arg $ no_minify_arg $ format_arg $ global_name_arg
    $ external_arg $ sourcemap_arg $ banner_arg $ banner_file_arg $ out_name_arg)

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
      `Pre "  fennec build --format iife --global-name App --external react src/widget.jsx" ]
  in
  Cmd.v (Cmd.info "build" ~doc ~man) build_term

let dev_cmd =
  let target_arg =
    let doc = "The dune target to build and watch (default: the whole project)." in
    Arg.(value & opt string "@@default" & info [ "target" ] ~docv:"TARGET" ~doc)
  in
  let exe_arg =
    let doc = "Path to the built server executable to run and supervise (under _build)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SERVER_EXE" ~doc)
  in
  let go target exe = Dev.run target exe in
  let doc = "Run the dev server with livereload" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Start a development loop: run $(b,dune build --watch) (dune is the sole source watcher \
         and builder — including assets, which are dune rules that call $(b,fennec build)) and \
         supervise the server executable, restarting it when a backend change rebuilds it. \
         Frontend-only edits live-reload without a restart via the framework's own asset \
         watcher.";
      `P
        "This command is pure convenience. The project is a plain dune project: $(b,dune build \
         --watch) plus $(b,dune exec) gives the same livereload without the CLI.";
      `S Manpage.s_examples;
      `Pre "  fennec dev _build/default/examples/helloworld/server.exe";
      `Pre "  fennec dev --target examples/helloworld/ _build/default/examples/helloworld/server.exe" ]
  in
  Cmd.v (Cmd.info "dev" ~doc ~man) Term.(const go $ target_arg $ exe_arg)

let embed_cmd =
  let dir_arg =
    let doc = "Directory to embed (e.g. public)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIR" ~doc)
  in
  let out_arg =
    let doc = "Output OCaml file (a module mapping path -> bytes)." in
    Arg.(value & opt string "public_assets.ml" & info [ "o"; "output" ] ~docv:"FILE" ~doc)
  in
  let doc = "Embed a directory tree into an OCaml module for compile-time bundling" in
  let man =
    [ `S Manpage.s_description;
      `P
        "Walk $(i,DIR) and emit an OCaml module mapping each relative file path to its bytes, so \
         the directory can be baked into the binary at compile time (prod) and served from \
         memory. Intended to be driven by a dune rule, exactly like $(b,fennec build). The \
         generated module exposes $(b,lookup : string -> string option) and $(b,paths : string \
         list).";
      `S Manpage.s_examples;
      `Pre "  fennec embed public -o public_assets.ml" ]
  in
  Cmd.v (Cmd.info "embed" ~doc ~man) Term.(const Embed.run $ dir_arg $ out_arg)

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
  Cmd.group info [ build_cmd; dev_cmd; embed_cmd ]

let () = exit (Cmd.eval' main_cmd)
