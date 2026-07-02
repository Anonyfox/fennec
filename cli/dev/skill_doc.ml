let render () =
  String.concat "\n"
    [ "# Fennec";
      "";
      "Fennec is an OCaml fullstack framework built on plain dune projects. Use the CLI as the source of truth for task discovery, builds, dev feedback, tests, docs, and application agent feedback.";
      "";
      "## Start Here";
      "";
      "```sh";
      "fennec                         # print this guide";
      "fennec discover \"TASK\"         # find the Fennec way before editing";
      "fennec dev                     # discover and run the app with live reload";
      "fennec console                 # REPL with the whole app + framework loaded (no HTTP)";
      "fennec test                    # fast unit gate";
      "fennec test all                # unit -> http -> browser -> system -> docs";
      "fennec test docs               # doc coverage, warn-only by default";
      "fennec build INPUT...          # native JS/CSS/SCSS bundling";
      "fennec release                 # build + verify + stage a production deployable";
      "fennec image in.jpg out.webp   # convert/resize/favicon images (no ImageMagick)";
      "fennec doctor                  # check the .mlx toolchain when a web app won't build";
      "```";
      "";
      "The project remains a normal dune project. `fennec` adds source-backed task discovery, restart/livereload, native asset bundling, app test orchestration, doc coverage, and agent-facing feedback.";
      "";
      "## Before Editing";
      "";
      "Ask the shipped framework snapshot what public path to use. This is task discovery, not symbol lookup.";
      "";
      "```sh";
      "fennec discover \"protect admin route with basic auth\"";
      "fennec discover \"build an SSR page with a local counter\"";
      "fennec discover --browse Paw";
      "fennec discover --why ID";
      "```";
      "";
      "Use the answer, starter snippet, receipts, and `Next` commands before guessing APIs from memory or model training data.";
      "";
      "## Dev Loop";
      "";
      "Run:";
      "";
      "```sh";
      "fennec dev";
      "```";
      "";
      "With no server path, Fennec asks dune for the executable that calls `Fennec.serve`. Dune remains the sole source watcher and builder. Backend changes restart the server; client changes reload or hot-swap CSS; build failures keep the last good server serving when possible.";
      "";
      "Useful variants:";
      "";
      "```sh";
      "fennec dev --dry-run";
      "fennec dev --port 9000";
      "fennec dev --clean";
      "```";
      "";
      "When `MONGO_URL` is unset, `fennec dev` generates a zero-config `burrow://` URL — a durable embedded database (data survives restarts) with a live mongosh-compatible endpoint, no external process. An explicit `MONGO_URL` (`:memory:` / `burrow://` / `mongodb://`) always wins.";
      "";
      "## Console";
      "";
      "`fennec console` boots the whole app — framework, your libraries, the data backend (the persistent burrow in dev) — into an OCaml REPL, like `iex -S mix` or `rails console`. Evaluate against the live runtime with the app's own modules in scope:";
      "";
      "```sh";
      "fennec console                 # standalone REPL (no HTTP server)";
      "fennec dev --console           # the dev loop AND a REPL pinned to the bottom of the feed";
      "```";
      "";
      "It runs the project's `console` target (a bytecode toplevel calling `Fennec.console_run`, never `Fennec.serve`, so server discovery ignores it). The backend is shared on disk, so what you query is the same data the dev server serves. Ctrl-C cancels a running eval; Ctrl-D leaves. Use it to probe collections, call a handler, or check an Accounts/Sift expression without writing a throwaway test.";
      "";
      "## Deploy";
      "";
      "`fennec release` produces the deployable: it discovers the server, builds it native under the `release` profile (inline tests stripped; a web app's JS/CSS/static web root baked into the binary), verifies the binary is prod-lean and that its web root actually embedded, strips it, and stages a single self-contained binary into `./dist` — then prints the runtime deploy contract.";
      "";
      "```sh";
      "fennec release                 # build, verify, strip, stage ./dist/<server>";
      "fennec release --check         # verify only (CI gate); stage nothing";
      "fennec release --docker        # also emit a runtime Dockerfile";
      "fennec release --target linux/amd64   # cross-build via Docker -> ./dist/linux-amd64/<server>";
      "```";
      "";
      "Plain `dune build --profile release` produces the same artifact; `fennec release` adds the incantation, the two silent-failure checks, and the run-time contract. The binary is production by default — Fennec keys dev/prod off bytecode-vs-native (the dev loop runs `server.bc`, a release is native `server.exe`), so there is NO mode flag to set. Run it with a `MONGO_URL` (`burrow:///abs/path/db` for embedded-durable, `mongodb://…` for hosted; a missing URL fails clearly rather than silently running in memory); `FENNEC_ENV=development` is an optional override to run the native build in dev mode.";
      "";
      "`--target <platform>` cross-builds via Docker (the binary is built INSIDE a target-platform container and extracted to `./dist/<os-arch>/`) — Docker reaches `linux/amd64` and `linux/arm64`; macOS/Windows build on a native host or CI. Add `--image TAG` for a deployable image, `--build-image` for a pre-baked toolchain image. First cross-build is slow (compiles the toolchain); buildx caches it.";
      "";
      "## Agent Fastlane";
      "";
      "For application projects, coding agents should attach once and let the devserver post feedback after edits. Do not run manual `dune build` / `dune runtest` probes after every application edit.";
      "";
      "Start one dev loop with an agent journal and ask Fennec to attach the current harness:";
      "";
      "```sh";
      "fennec dev --agent --attach --port 9123";
      "```";
      "";
      "Attach installs one guarded user-level post-tool hook when the active harness supports it. The hook command is:";
      "";
      "```sh";
      "fennec agent hook --timeout 12";
      "```";
      "";
      "After attach, edit application files normally. The first edit that reaches the hook injects model context containing `Fennec dev feedback after this tool`. Do not compensate by tailing logs or running build/test probes after every application edit.";
      "";
      "The hook is stateful: it returns the next undelivered Fennec verdict, including feedback that settled before the hook process started. Do not call separate wait commands after edits. Do not parse terminal output.";
      "";
      "The agent verdict includes served change, inline-test result when tests ran, inferred affected surface, timing fields, and focused compiler diagnostics with file/line/code frame when a build fails.";
      "";
      "Runtime HTTP errors (any 5xx, or a request that fell through the error funnel) are tracked separately from build/test verdicts. New ones since your last tool are appended to the same post-tool feedback (`N runtime HTTP error(s) since your last check`), so you see async failures without polling. The full session list:";
      "";
      "```sh";
      "fennec agent errors            # status · method · path · timing · message, one row per failure";
      "fennec agent errors --after ID # only failures newer than ID";
      "```";
      "";
      "Recovery:";
      "";
      "```sh";
      "fennec agent status";
      "```";
      "";
      "`status` reports the journal, latest verdict, configured port, and whether the recorded devserver pid is alive.";
      "";
      "Framework and monorepo development is different: do not attach fastlane while editing Fennec itself. Use focused `dune build` / `dune runtest` checks for the package or module you changed.";
      "";
      "## Tests";
      "";
      "```sh";
      "fennec test              # unit";
      "fennec test http";
      "fennec test browser";
      "fennec test system";
      "fennec test docs --strict";
      "fennec test new system NAME";
      "```";
      "";
      "Use the narrowest cut that proves the change. `all` is the full local confidence pass.";
      "";
      "## Agent Rules";
      "";
      "- Before using unfamiliar Fennec APIs, run `fennec discover \"TASK\"`; do not guess from training data.";
      "- Prefer `fennec dev --agent --attach` for iterative application coding; it installs the guarded post-tool hook for the active harness.";
      "- When editing the Fennec framework or monorepo itself, do not attach fastlane; use focused Dune checks.";
      "- Trust hook verdicts for compile/test/reload feedback; they are generated by the devserver, not by terminal scraping.";
      "- Treat attach as live after an edit produces `Fennec dev feedback after this tool` in your model context.";
      "- On build failure, fix the reported focused diagnostic first.";
      "- Do not run explicit wait commands as the normal loop; use `fennec agent status` only for recovery.";
      "- Avoid adding local harness config or root instruction files unless the user explicitly asks.";
      "- Before final handoff, run the relevant `fennec test ...` cut or explain why it was not run.";
      "" ]

(* ── human rendering ───────────────────────────────────────────────────────────────────────────
   The SAME guide markdown, pretty-printed for a person at a terminal: brand-orange headers, cyan
   commands, dimmed inline comments, no raw `#`/```/backtick noise. [render] stays the canonical
   markdown an agent consumes ([fennec skill]); [render_human] is what bare [fennec] prints — and it
   degrades to the raw markdown the moment colour is off (piped, NO_COLOR, not a TTY), so a redirect
   never gets escape soup. One source of truth; two faithful renderings. *)

(* recolour `inline code` spans in prose: drop the backticks, tint the inner text *)
let tint_inline tint s =
  let b = Buffer.create (String.length s) and n = String.length s and i = ref 0 in
  while !i < n do
    if s.[!i] = '`' then (
      match String.index_from_opt s (!i + 1) '`' with
      | Some j -> Buffer.add_string b (tint (String.sub s (!i + 1) (j - !i - 1))); i := j + 1
      | None -> Buffer.add_char b '`'; incr i)
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

let pretty (t : Tty.t) (md : string) : string =
  let bold s = Tty.sgr t "1" s and dim s = Tty.sgr t "2" s in
  let brand s = Tty.sgr t "38;5;173" s (* warm terracotta, the Fennec brand *) and cyan s = Tty.sgr t "36" s in
  let has p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p in
  let drop n s = String.sub s n (String.length s - n) in
  (* a code line "cmd        # note" → command tinted, the aligned trailing comment dimmed; indent 2 *)
  let code_line line =
    let n = String.length line in
    let rec hash i = if i >= n then None else if line.[i] = '#' && i > 0 && line.[i - 1] = ' ' then Some i else hash (i + 1) in
    match hash 0 with
    | Some k -> "  " ^ cyan (String.sub line 0 k) ^ dim (drop k line)
    | None -> "  " ^ cyan line
  in
  let in_code = ref false in
  String.split_on_char '\n' md
  |> List.filter_map (fun line ->
         if has "```" (String.trim line) then (in_code := not !in_code; None) (* drop fence markers *)
         else if !in_code then Some (code_line line)
         else if has "## " line then Some (brand "▌ " ^ bold (brand (drop 3 line)))
         else if has "# " line then Some (bold (brand (drop 2 line)))
         else if has "- " line then Some ("  " ^ brand "•" ^ " " ^ tint_inline cyan (drop 2 line))
         else Some (tint_inline cyan line))
  |> String.concat "\n"

(* bare [fennec]: pretty when a person is watching a colour terminal, raw markdown otherwise *)
let render_human () =
  let t = Tty.detect () in
  if t.color then pretty t (render ()) else render ()

let%test "human render is the raw guide when colour is off (pipes/agents/tests)" =
  render_human () = render ()

let%test "pretty render tints headers + strips markdown noise, keeps the content" =
  let t = { Tty.color = true; hyperlinks = false; interactive = true; width = 80 } in
  let p = pretty t (render ()) in
  Fennec_hunt_unit.str_contains p "\027[" (* emitted ANSI *)
  && Fennec_hunt_unit.str_contains p "Fennec"
  && Fennec_hunt_unit.str_contains p "fennec discover"
  && not (Fennec_hunt_unit.str_contains p "```")  (* fence markers gone *)
  && not (Fennec_hunt_unit.str_contains p "## ")   (* header hashes gone *)

let%test "guide names the agent hook" =
  Fennec_hunt_unit.str_contains (render ()) "fennec agent hook --timeout 12"

let%test "guide names attach path" =
  Fennec_hunt_unit.str_contains (render ()) "fennec dev --agent --attach --port 9123"

let%test "guide teaches pre-edit discovery" =
  Fennec_hunt_unit.str_contains (render ()) "fennec discover \"TASK\""
  && Fennec_hunt_unit.str_contains (render ()) "## Before Editing"

let%test "guide tells agents not to guess unknown apis" =
  Fennec_hunt_unit.str_contains (render ()) "do not guess from training data"

let%test "guide tells agents not to poll builds after hooks" =
  Fennec_hunt_unit.str_contains (render ()) "Do not run manual `dune build` / `dune runtest` probes after every application edit"

let%test "guide excludes framework work from fastlane" =
  Fennec_hunt_unit.str_contains (render ()) "do not attach fastlane while editing Fennec itself"
  && Fennec_hunt_unit.str_contains (render ()) "use focused Dune checks"

let%test "guide makes hook the only normal fastlane path" =
  Fennec_hunt_unit.str_contains (render ()) "Fennec dev feedback after this tool"
  && Fennec_hunt_unit.str_contains (render ()) "Do not run explicit wait commands"
  && not (Fennec_hunt_unit.str_contains (render ()) "fennec agent wait --after")
