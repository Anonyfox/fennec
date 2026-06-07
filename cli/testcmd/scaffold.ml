(* `fennec test new <cut> <name>` — scaffold a suite so even the FIRST file in a new cut dir is
   zero-friction. Creates the convention dune + the one-line runner (once per dir) and a starter
   suite. After that, adding suites is just dropping more `*_test.ml` files (the -linkall library
   picks them up — no dune edit).

   The templates are pure (string-returning) functions, unit-tested; only [create] touches disk. *)

let cuts = [ "http"; "browser"; "system" ]

(* a clean OCaml module name from a cut + the user's name, e.g. http + "checkout" → the suite file
   "checkout_test.ml" and a unique library "http_suites". *)
let suite_file name = name ^ "_test.ml"
let library_name cut = cut ^ "_suites"

(* the one-time dune: a -linkall library of every *_test.ml + the one-line `run` entry. *)
let dune_template cut =
  let lib = library_name cut in
  let chrome_note = if cut = "browser" then " (needs a system Chrome)" else "" in
  Printf.sprintf
    {|; %s suites for `fennec test %s`%s. Authoring is zero-ceremony: drop a `*_test.ml` with a
; `let%%%s` block — no main, no edit here. The -linkall library force-links every suite module so
; each registers; the one-line `run` executable is the entry `fennec test %s` builds and runs.
(library
 (name %s)
 (modules (:standard \ run))
 (libraries fennec-hunt)
 (library_flags (-linkall))
 (preprocess (pps fennec-hunt.ppx)))

(executable
 (name run)
 (modules run)
 (libraries %s fennec-hunt))
|}
    (String.capitalize_ascii cut) cut chrome_note cut cut lib lib

(* the one-line runner entry per cut. *)
let run_template = function
  | "http" -> "let () = exit (Fennec_hunt.Http.run ())\n"
  | "browser" -> "let () = Fennec_hunt.Run.main_cli ()\n"
  | "system" -> "let () = exit (Fennec_hunt.System.run ())\n"
  | _ -> ""

(* a starter suite that compiles and passes against the example-style app. *)
let starter_template cut name =
  match cut with
  | "http" ->
    Printf.sprintf
      {|(* An Http suite — assert on responses, no browser. Drop more `let%%http` blocks here, or more
   `*_test.ml` files in this directory. No ~url: `fennec test http` boots an isolated instance per
   suite and hands it over. *)
open Fennec_hunt.Http

let%%http %S = fun () ->
  check "home page responds" (fun () -> get "/" ~expect:[ status 200 ])
|}
      name
  | "browser" ->
    Printf.sprintf
      {|(* A Browser suite — drive a real headless Chrome over CDP. Each test gets a fresh page. Drop
   more `let%%browser` blocks here, or more `*_test.ml` files in this directory. *)
open Fennec_hunt.Live

let%%browser %S = fun page ->
  page |> goto "/" |> expect_text "h1" "" |> ignore
|}
      name
  | "system" ->
    Printf.sprintf
      {|(* A System suite — drives the real `fennec dev` and asserts its observable behaviour. The
   sandbox reaps the whole process group on teardown (no orphans); waits are deadline-bounded. *)
module S = Fennec_hunt.System

let%%system %S = fun sb ->
  let dev = S.dev sb in              (* spawns THIS app's `fennec dev` — typed, no env strings *)
  S.wait_ready dev ~port:4000 ();
  S.check "the dev gateway is serving" (S.port_open 4000)
|}
      name
  | _ -> ""

(* the three files a scaffold writes, as (relative-path, contents). [dir] is the cut dir. *)
let files ~dir ~cut ~name =
  [ (Filename.concat dir "dune", dune_template cut);
    (Filename.concat dir "run.ml", run_template cut);
    (Filename.concat dir (suite_file name), starter_template cut name) ]

(* validate the request, purely: a known cut + a non-empty, identifier-safe name. *)
let validate ~cut ~name : (unit, string) result =
  if not (List.mem cut cuts) then
    Error (Printf.sprintf "unknown cut %S — expected one of: %s" cut (String.concat ", " cuts))
  else if name = "" then Error "a suite name is required: fennec test new <cut> <name>"
  else if not (String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_') name) then
    Error (Printf.sprintf "suite name %S must be a lowercase identifier (a-z, 0-9, _)" name)
  else Ok ()

(* Create the scaffold under [cwd]: make [test/<cut>/], write the dune + run.ml (only if absent —
   so a second `new` in the same cut just adds a suite), and the starter suite (error if it already
   exists). Returns the files actually created (cwd-relative). The only disk-touching part. *)
let create ~cwd ~cut ~name : (string list, string) result =
  match validate ~cut ~name with
  | Error _ as e -> e
  | Ok () ->
    let reldir = Filename.concat "test" cut in
    let suite_rel = Filename.concat reldir (suite_file name) in
    if Sys.file_exists (Filename.concat cwd suite_rel) then
      Error (Printf.sprintf "%s already exists" suite_rel)
    else begin
      (try Unix.mkdir (Filename.concat cwd "test") 0o755 with _ -> ());
      (try Unix.mkdir (Filename.concat cwd reldir) 0o755 with _ -> ());
      let created =
        List.filter_map
          (fun (rel, contents) ->
            let path = Filename.concat cwd rel in
            if Sys.file_exists path then None
            else (Out_channel.with_open_bin path (fun oc -> output_string oc contents); Some rel))
          (files ~dir:reldir ~cut ~name)
      in
      Ok created
    end

(* ──── tests (pure) ──── *)

let%test "library name" = library_name "http" = "http_suites"
let%test "suite file" = suite_file "checkout" = "checkout_test.ml"
let%test "run template per cut" =
  Fennec_hunt_unit.str_contains (run_template "http") "Http.run"
  && Fennec_hunt_unit.str_contains (run_template "system") "System.run"
  && Fennec_hunt_unit.str_contains (run_template "browser") "main_cli"
let%test "dune template names the library + run exe" =
  let d = dune_template "system" in
  Fennec_hunt_unit.str_contains d "(name system_suites)" && Fennec_hunt_unit.str_contains d "(name run)"
  && Fennec_hunt_unit.str_contains d "-linkall"
let%test "starter uses the right ppx + the given name" =
  Fennec_hunt_unit.str_contains (starter_template "http" "checkout") "let%http \"checkout\""
  && Fennec_hunt_unit.str_contains (starter_template "system" "boots") "let%system \"boots\""
let%test "validate: good" = validate ~cut:"http" ~name:"checkout" = Ok ()
let%test "validate: unknown cut names the set" =
  match validate ~cut:"bogus" ~name:"x" with Error m -> Fennec_hunt_unit.str_contains m "http, browser, system" | Ok () -> false
let%test "validate: empty name rejected" = (match validate ~cut:"http" ~name:"" with Error _ -> true | Ok () -> false)
let%test "validate: bad chars rejected" = (match validate ~cut:"http" ~name:"Check-Out" with Error _ -> true | Ok () -> false)
let%test "files: three of them, suite last" =
  match files ~dir:"test/http" ~cut:"http" ~name:"checkout" with
  | [ (a, _); (b, _); (c, _) ] -> a = "test/http/dune" && b = "test/http/run.ml" && c = "test/http/checkout_test.ml"
  | _ -> false
