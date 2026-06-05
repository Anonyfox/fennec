(* Http tests for the example site — fennec_hunt.Http dogfoods itself.
   Tests the server as a black box via its URL. No fennec internals, no dune coupling. *)

open Fennec_hunt.Http

let site = let d = Sys.getcwd () in
  if Sys.file_exists (Filename.concat d "examples/site/server.ml") then Filename.concat d "examples/site"
  else if Sys.file_exists "server.ml" then d
  else failwith "run from the repo root or examples/site"

let opam_bin = try let ic = Unix.open_process_in "opam var bin 2>/dev/null" in
  let v = String.trim (input_line ic) in ignore (Unix.close_process_in ic); v with _ -> ""

let fennec_cmd =
  ["sh"; "-c"; Printf.sprintf "cd '%s' && exec fennec dev" site]

let base_env =
  let path = let cur = try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin" in
    if opam_bin = "" then cur else opam_bin ^ ":" ^ cur in
  [| "FENNEC_DEV_LIVERELOAD=0"; "PATH=" ^ path |]

let () = hunt "site server" ~url:"http://localhost:4000" ~spawn:fennec_cmd ~env:base_env @@ fun () ->

  check "web health endpoint" (fun () ->
    get "/api/health" ~expect:[status 200; is_json; body_contains {|"ok":true|}]);

  check "web home page" (fun () ->
    get "/" ~expect:[status 200; is_html; body_contains "Welcome to the Fennec site"]);

  check "admin without auth → 401 (matched-phase)" (fun () ->
    get "/" ~host:"admin.localhost" ~expect:[status 401]);

  check "admin with basic auth → 200" (fun () ->
    get "/" ~host:"admin.localhost" ~headers:[basic_auth "admin" "admin"]
      ~expect:[status 200; body_contains "Admin Dashboard"]);

  check "unknown host falls to web default" (fun () ->
    get "/" ~host:"random.example.com"
      ~expect:[status 200; body_contains "Welcome to the Fennec site"]);

  check "API returns JSON with app identifier" (fun () ->
    get "/api/health" ~expect:[is_json; body_contains {|"app":"web"|}]);

  check "streaming endpoint" (fun () ->
    get "/api/stream" ~expect:[status 200; body_contains "chunk-1"; body_contains "chunk-2"; body_contains "chunk-3"]);

  check "response time is sane" (fun () ->
    get "/api/health" ~expect:[status 200; max_elapsed 500.0])
