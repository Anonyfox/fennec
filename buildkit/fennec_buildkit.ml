(** Fennec Buildkit — an in-process JavaScript bundler (esbuild) and CSS/SCSS
    processor (Lightning CSS + grass), statically linked into your OCaml binary as
    native libraries. No node, no subprocess, no second-language runtime: just
    OCaml calls into warm Go/Rust code.

    {1 Why}

    Driving a frontend build pipeline from OCaml usually means shelling out to
    node tooling. Buildkit removes that entirely — the bundler and CSS engine are
    linked in, so [Esbuild.build]/[Css.scss] are plain function calls. The native
    archives are compiled for your platform automatically at build time; consumers
    of this library need no extra steps.

    {1 Example}
    {[
      (* one-shot production bundle *)
      let js = Fennec_buildkit.Esbuild.build ~entry:"app.js" ~minify:true () in

      (* a warm context for fast incremental rebuilds (dev servers) *)
      let ctx = Fennec_buildkit.Esbuild.create ~entry:"app.js" () in
      let js = Fennec_buildkit.Esbuild.rebuild ctx in    (* ~ms, reuses parse work *)
      Fennec_buildkit.Esbuild.dispose ctx;

      (* SCSS -> minified CSS *)
      let css = Fennec_buildkit.Css.scss ~minify:true ".a{ &:hover{ color:red } }" in
    ]} *)

module Esbuild = struct
  (** An opaque, warm build context. Subsequent {!rebuild}s reuse parse work, so
      incremental rebuilds are typically single-digit milliseconds. *)
  type ctx = int

  external _create : string -> int = "fennec_bk_ctx_create"
  external _rebuild : int -> string = "fennec_bk_ctx_rebuild"
  external _dispose : int -> unit = "fennec_bk_ctx_dispose"

  let json_opts ~entry ~format ~global_name ~external_ ~minify ~sourcemap ~banner =
    let ext = String.concat "," (List.map (Printf.sprintf "%S") external_) in
    Printf.sprintf
      {|{"entry":%S,"format":%S,"globalName":%S,"external":[%s],"minify":%b,"sourcemap":%b,"banner":%S}|}
      entry format global_name ext minify sourcemap banner

  (** Create a build context.
      @param entry path to the entry module (bundling follows its imports).
      @param format ["iife"] (default), ["esm"], or ["cjs"].
      @param global_name IIFE global to expose the entry's exports as.
      @param external_ import paths to leave unbundled (e.g. ["react"]).
      @param minify minify whitespace + identifiers + syntax.
      @param sourcemap inline source map.
      @param banner text prepended to the output (e.g. a runtime shim). *)
  let create ?(format = "iife") ?(global_name = "") ?(external_ = []) ?(minify = false)
      ?(sourcemap = false) ?(banner = "") ~entry () : ctx =
    let h = _create (json_opts ~entry ~format ~global_name ~external_ ~minify ~sourcemap ~banner) in
    if h < 0 then failwith "fennec-buildkit: failed to create esbuild context";
    h

  (** Create a context directly from a pre-encoded options JSON (as produced by
      {!json_opts}). Used by the dev build worker so a warm rebuild uses the exact
      same options — and therefore yields byte-identical output — as the cold path. *)
  let create_json (opts_json : string) : ctx =
    let h = _create opts_json in
    if h < 0 then failwith "fennec-buildkit: failed to create esbuild context";
    h

  (** Rebuild the bundle, returning the bytes.
      @raise Failure with esbuild's formatted message on a build error. *)
  let rebuild (c : ctx) : string = _rebuild c

  (** Release a context. *)
  let dispose (c : ctx) : unit = _dispose c

  (** One-shot: create, build once, dispose; returns the bundle bytes. *)
  let build ?format ?global_name ?external_ ?minify ?sourcemap ?banner ~entry () : string =
    let c = create ?format ?global_name ?external_ ?minify ?sourcemap ?banner ~entry () in
    Fun.protect ~finally:(fun () -> dispose c) (fun () -> rebuild c)
end

module Css = struct
  external _transform : string -> int -> string = "fennec_bk_css"
  external _scss : string -> int -> string = "fennec_bk_scss"
  external _scss_path : string -> int -> string = "fennec_bk_scss_path"

  (** Optimize modern CSS — flatten nesting, reduce [calc()], dedupe, and
      (optionally) minify. *)
  let transform ?(minify = true) (src : string) : string = _transform src (if minify then 1 else 0)

  (** Compile SCSS source text (variables, mixins, [@for], functions,
      interpolation) to CSS via grass, then optimize/minify via Lightning CSS. *)
  let scss ?(minify = true) (src : string) : string = _scss src (if minify then 1 else 0)

  (** Compile a SCSS *file* by path: [@use]/[@import] resolve relative to the
      file (so a component's stylesheet can live next to it and be pulled in by
      an app's entry sheet), then optimize/minify. *)
  let scss_path ?(minify = true) (path : string) : string =
    _scss_path path (if minify then 1 else 0)
end

(** Cross-platform filesystem events (the Rust [notify] crate: FSEvents / inotify
    / kqueue / Windows). Lets a tool react to a finished build the instant the OS
    reports it — no polling. A handle is an opaque pointer carried as a nativeint;
    [0n] means watching is unavailable on this platform, so the caller should fall
    back to a poll. *)
module Watch = struct
  type t = nativeint

  external _start : string -> int -> nativeint = "fennec_bk_watch_start"
  external _add : nativeint -> string -> int -> int = "fennec_bk_watch_add"
  external _wait : nativeint -> int -> int = "fennec_bk_watch_wait"
  external _free : nativeint -> unit = "fennec_bk_watch_free"

  (** Watch [dir] for filesystem events. [recursive] defaults to false. *)
  let start ?(recursive = false) (dir : string) : t = _start dir (if recursive then 1 else 0)

  (** Add another [dir] to an existing watcher; all paths feed one event stream.
      Returns whether it was added. *)
  let add (h : t) ?(recursive = false) (dir : string) : bool =
    _add h dir (if recursive then 1 else 0) = 1

  (** Whether a watcher was created; if not, poll instead. *)
  let available (h : t) : bool = h <> 0n

  (** Block until an event fires or [timeout_ms] elapses; [true] iff an event
      fired. The blocking wait releases the OCaml runtime lock. *)
  let wait (h : t) ~timeout_ms : bool = _wait h timeout_ms = 1

  let free (h : t) : unit = _free h
end
