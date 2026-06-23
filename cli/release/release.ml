(* See release.mli. The pipeline that ties the focused modules together:

     Target.resolve  →  Build.run  →  gate (Lean + Verify)  →  Stage.run  →  Contract

   Each step short-circuits with an actionable message; only a fully built, prod-lean, embed-verified
   artifact gets staged. The gate is the value: it refuses to ship a binary that links the heavy test
   machinery (Lean) or that compiled fine but baked in no web root (Verify) — the two silent prod
   failures a plain `dune build --profile release` would let through.

   I/O contract: progress + the deploy contract go to STDOUT (so `--check`'s verdict and the contract
   are pipeable in CI); every diagnostic + warning goes to STDERR (via {!err} / [Printf.eprintf]). *)

type opts = {
  outdir : string;
  docker : bool;
  strip : bool;
  check_only : bool;
  targets : string list;  (* --target; empty ⇒ build for THIS host (no Docker) *)
  image : string option;  (* --image TAG; with --target, emit a runtime image instead of a loose binary *)
  build_image : string option;  (* --build-image; the cross-build base (default Cross.default_base) *)
}

let err fmt = Printf.ksprintf (fun s -> Printf.eprintf "fennec release: %s\n%!" s; 1) fmt

(* the build + verification gate: [Ok embedded] (embedded = file count, or None for an API/SSR-only
   server), or [Error message] naming exactly what is wrong and how to fix it. *)
let gate (t : Target.t) : (int option, string) result =
  match Lean.scan ~exe:t.Target.built_exe with
  | Error msg -> Error (Printf.sprintf "could not read the built binary at %s: %s" t.Target.built_exe msg)
  | Ok (Lean.Leaked needles) ->
    Error
      (Printf.sprintf
         "the binary is not prod-lean — it links heavy test machinery: %s.\n\
         \  A production server must not carry the CDP/Chrome/yojson test weight; check that no server\n\
         \  depends on `fennec-hunt` (directly or transitively)."
         (String.concat ", " needles))
  | Ok Lean.Clean -> (
    match Verify.assets t with
    | Verify.Embedded n -> Ok (Some n)
    | Verify.No_webroot -> Ok None
    | Verify.Stub ->
      Error
        "built --profile release but assets.ml is the dev stub — the web-root embed rule did not run.\n\
        \  Add the release embed rule to your app's dune (next to the webroot rule):\n\
        \    (rule (target assets.ml) (enabled_if (= %{profile} release)) (deps webroot)\n\
        \     (action (run %{bin:fennec} build -o webroot --embed assets.ml)))")

(* stage the artifact + emit the contract (and Dockerfile). Reached only once the gate is green. *)
let finish opts (t : Target.t) (embedded : int option) : int =
  match Stage.run ~built_exe:t.Target.built_exe ~outdir:opts.outdir ~name:t.Target.name ~strip:opts.strip with
  | Error e -> err "staging failed: %s" e
  | Ok staged ->
    if opts.docker then begin
      (* Contract.dockerfile normalizes the COPY path (drops a leading ./), so we pass it raw *)
      let bin = Filename.concat opts.outdir t.Target.name in
      try
        Out_channel.with_open_bin "Dockerfile" (fun oc ->
            Out_channel.output_string oc (Contract.dockerfile ~name:t.Target.name ~bin));
        Printf.printf "  wrote ./Dockerfile\n"
      with Sys_error msg -> Printf.eprintf "fennec release: could not write Dockerfile: %s\n%!" msg
    end;
    print_string (Contract.render ~path:staged.Stage.path ~bytes:staged.Stage.bytes ~embedded);
    0

let run_host opts : int =
  match Target.resolve () with
  | Error e -> err "%s" e
  | Ok t -> (
    Printf.printf "fennec release: building %s (profile %s)…\n%!" t.Target.name Fennec_dev.Build_dir.release_profile;
    match Build.run ~root:t.Target.root ~target:t.Target.exe_target with
    | Error e -> err "%s" e
    | Ok () -> (
      match gate t with
      | Error msg -> err "%s" msg
      | Ok embedded ->
        if opts.check_only then begin
          Printf.printf "fennec release: ✓ %s is prod-lean and %s (checked — nothing staged)\n%!" t.Target.name
            (match embedded with Some n -> Printf.sprintf "embeds %d asset(s)" n | None -> "API/SSR-only");
          0
        end
        else finish opts t embedded))

(* ── cross-platform builds (Docker) ─────────────────────────────────────────────────────────────── *)

(* per-Dockerfile ignore so the build context skips the heavy / irrelevant trees (BuildKit reads
   `<dockerfile>.dockerignore`); without it `COPY . .` would ship _build/.git/node_modules into Docker *)
let dockerignore = "_build\n.git\ndist\n.fennec\nnode_modules\n_opam\n*.install\n"

(* one target: write the generated Dockerfile (+ its .dockerignore) under .fennec/release/, run buildx
   from the workspace root so the whole project is the build context. Docker's output streams through. *)
let cross_one ~root ~base ~(t : Target.t) ~(output : Cross.output) (p : Cross.platform) : bool =
  let dir = Filename.concat root ".fennec/release" in
  Util.mkdir_p dir;
  let dockerfile = Filename.concat dir (Cross.slug p ^ ".Dockerfile") in
  let write path s = Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc s) in
  write dockerfile (Cross.build_dockerfile ~base ~exe_target:t.Target.exe_target ~name:t.Target.name ~output);
  write (dockerfile ^ ".dockerignore") dockerignore;
  Printf.printf "fennec release: cross-building %s for %s …\n%!" t.Target.name (Cross.docker_platform p);
  Sys.command (Printf.sprintf "cd %s && %s" (Filename.quote root) (Cross.buildx_command ~platform:p ~dockerfile ~context:root ~output)) = 0

let run_cross opts (targets : Cross.platform list) (t : Target.t) : int =
  match Cross.docker_available () with
  | Error e -> err "%s" e
  | Ok () ->
    let base = Option.value opts.build_image ~default:Cross.default_base in
    let multi = List.length targets > 1 in
    let output_for (p : Cross.platform) : Cross.output =
      match opts.image with
      | Some tag -> Cross.To_image (if multi then tag ^ "-" ^ p.Cross.arch else tag)
      | None -> Cross.To_dir (Filename.concat opts.outdir (Cross.slug p))
    in
    let results = List.map (fun p -> let out = output_for p in (p, out, cross_one ~root:t.Target.root ~base ~t ~output:out p)) targets in
    Printf.printf "\n";
    List.iter
      (fun (p, out, ok) ->
        let where = match out with Cross.To_image tag -> "image " ^ tag | Cross.To_dir d -> Filename.concat d t.Target.name in
        Printf.printf "  %s  %s → %s\n" (if ok then "\xe2\x9c\x93" else "\xe2\x9c\x97") (Cross.docker_platform p) where)
      results;
    let failed = List.filter (fun (_, _, ok) -> not ok) results in
    if failed = [] then 0
    else (Printf.eprintf "fennec release: %d of %d target(s) failed (see the build output above)\n%!" (List.length failed) (List.length targets); 1)

let run opts : int =
  if opts.targets = [] then
    if opts.image <> None then err "--image requires --target (it builds the image inside Docker for a target platform)"
    else run_host opts
  else
    match Cross.parse_targets opts.targets with
    | Error e -> err "%s" e
    | Ok targets -> ( match Target.resolve () with Error e -> err "%s" e | Ok t -> run_cross opts targets t)
