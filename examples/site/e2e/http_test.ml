(* Http tests for the example site — the fennec_hunt Http layer dogfoods itself.
   Each [check] is a self-contained test case. The server is spawned once by [hunt]. *)

open Fennec_hunt.Http

(* fennec dev must run from the site directory (discovery scopes to cwd) *)
let site_dir =
  let d = Sys.getcwd () in
  if Sys.file_exists "server.ml" then d
  else if Sys.file_exists (Filename.concat d "examples/site/server.ml") then Filename.concat d "examples/site"
  else failwith "run from the repo root or examples/site"

(* opam bin must be on PATH — dune exec sandboxes it away *)
let opam_bin =
  try
    let ic = Unix.open_process_in "opam var bin 2>/dev/null" in
    let v = String.trim (input_line ic) in
    ignore (Unix.close_process_in ic); v
  with _ -> ""

let path_with_opam =
  let cur = try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin" in
  if opam_bin = "" then cur else opam_bin ^ ":" ^ cur

let () = hunt "site server (gateway :4000)"
  ~cmd:["sh"; "-c"; Printf.sprintf "cd '%s' && exec fennec dev" site_dir]
  ~port:4000
  ~env:[| "FENNEC_DEV_LIVERELOAD=0"; "PATH=" ^ path_with_opam |]
  @@ fun () ->

  check "web health endpoint" @@ fun () ->
    get "/api/health" ~expect:[status 200; body_contains {|"ok":true|}]
  ;

  check "web home page" @@ fun () ->
    get "/" ~expect:[status 200; body_contains "Welcome to the Fennec site"]
  ;

  check "admin without auth → 401 (matched-phase)" @@ fun () ->
    get "/" ~host:"admin.localhost" ~expect:[status 401]
  ;

  check "admin with basic auth → 200" @@ fun () ->
    get "/" ~host:"admin.localhost" ~headers:[basic_auth "admin" "admin"]
      ~expect:[status 200; body_contains "Admin Dashboard"]
  ;

  check "unknown host → web default" @@ fun () ->
    get "/" ~host:"random.example.com"
      ~expect:[status 200; body_contains "Welcome to the Fennec site"]
  ;

  check "web API route on gateway" @@ fun () ->
    get "/api/health" ~expect:[body_contains {|"app":"web"|}]
