(* The Http testing layer — bare functions that read from an ambient context.

   {[open Fennec_hunt.Http

     let () = hunt "site server" ~cmd:["fennec"; "dev"] ~port:4001 @@ fun () ->

       check "health endpoint" @@ fun () ->
         get "/health" ~expect:[status 200; body_contains "ok"]
       ;

       check "admin needs auth" @@ fun () ->
         get "/admin" ~expect:[status 401];
         get "/admin" ~headers:[basic_auth "admin" "admin"] ~expect:[status 200]
   ]}

   [hunt] spawns a server, sets up the ambient context, runs the body, tears down.
   [check] is a labeled test case. [get], [post], etc. make requests and optionally assert.
   [status], [body_contains], etc. are assertion constructors for [~expect].
   [header], [elapsed_ms], etc. extract from the last response. All bare — no prefix. *)

(* ---- types ---- *)

type response = Http_client.response = { status : int; headers : (string * string) list; body : string }
type assertion = response -> unit

(* ---- ambient context ---- *)

type context = {
  port : int;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  mutable last : response option;
  server_pid : int option;
  mutable checks_passed : int;
  mutable checks_failed : int;
}

let preview s = if String.length s <= 200 then s else String.sub s 0 197 ^ "..."

let ctx : context option ref = ref None
let current () = match !ctx with Some c -> c | None -> failwith "fennec_hunt: not inside a `hunt` block"

(* ---- timing ---- *)

let last_elapsed = ref 0.0

(* ---- assertions (response -> unit, for ~expect lists) ---- *)

let status code (r : response) =
  if r.status <> code then
    failwith (Printf.sprintf "expected status %d, got %d\n  body: %s" code r.status (preview r.body))

let status_2xx (r : response) =
  if r.status < 200 || r.status > 299 then
    failwith (Printf.sprintf "expected status 2xx, got %d\n  body: %s" r.status (preview r.body))

let body_contains needle (r : response) =
  if not (Fennec_hunt_util.contains r.body needle) then
    failwith (Printf.sprintf "expected body to contain %S\n  body: %s" needle (preview r.body))

let body_is expected (r : response) =
  if r.body <> expected then
    failwith (Printf.sprintf "expected body %S\n  got: %s" expected r.body)

let header_is name expected (r : response) =
  match Http_client.header_value name r with
  | Some v when v = expected -> ()
  | Some v -> failwith (Printf.sprintf "expected header %S = %S, got %S" name expected v)
  | None -> failwith (Printf.sprintf "expected header %S = %S, but header is absent" name expected)

let header_contains name needle (r : response) =
  match Http_client.header_value name r with
  | Some v when Fennec_hunt_util.contains v needle -> ()
  | Some v -> failwith (Printf.sprintf "expected header %S to contain %S, got %S" name needle v)
  | None -> failwith (Printf.sprintf "expected header %S to contain %S, but header is absent" name needle)

(* ---- request functions ---- *)

let run_expect r = function None -> () | Some checks -> List.iter (fun (check : assertion) -> check r) checks

let get ?(headers = []) ?host ?expect path =
  let c = current () in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.get ~net:c.net ~port:c.port ~headers path in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  run_expect r expect

let post ?(headers = []) ?host ?(body = "") ?expect path =
  let c = current () in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.post ~net:c.net ~port:c.port ~headers ~body path in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  run_expect r expect

let put ?(headers = []) ?host ?(body = "") ?expect path =
  let c = current () in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.put ~net:c.net ~port:c.port ~headers ~body path in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  run_expect r expect

let delete ?(headers = []) ?host ?expect path =
  let c = current () in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.delete ~net:c.net ~port:c.port ~headers path in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  run_expect r expect

(* ---- extractors (read from last response) ---- *)

let last () = match (current ()).last with Some r -> r | None -> failwith "fennec_hunt: no response yet (make a request first)"
let header name = match Http_client.header_value name (last ()) with Some v -> v | None -> failwith (Printf.sprintf "header %S not found in last response" name)
let response_body () = (last ()).body
let response_status () = (last ()).status
let elapsed_ms () = !last_elapsed

(* ---- helpers ---- *)

let basic_auth user pass =
  ("Authorization", "Basic " ^ Base64.encode_string (user ^ ":" ^ pass))

(* ---- process lifecycle ---- *)

let signal sig_ =
  match (current ()).server_pid with
  | Some pid -> (try Unix.kill pid sig_ with _ -> ())
  | None -> failwith "fennec_hunt: no server process (hunt was called without ~cmd)"

let wait_port_free ?(timeout = 10.0) () =
  Test_server.port_free_within ~timeout (current ()).port

let port_held () = Test_server.port_held (current ()).port

(* ---- check (a labeled test case) ---- *)

let check label body =
  let c = current () in
  c.last <- None;
  let t0 = Unix.gettimeofday () in
  match body () with
  | () ->
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    c.checks_passed <- c.checks_passed + 1;
    Printf.printf "  \027[32m✓\027[0m  %s \027[2m(%.0fms)\027[0m\n%!" label ms
  | exception e ->
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    c.checks_failed <- c.checks_failed + 1;
    Printf.printf "  \027[31m✗\027[0m  %s \027[2m(%.0fms)\027[0m\n%!" label ms;
    Printf.printf "     %s\n%!" (Printexc.to_string e)

(* ---- hunt (the top-level block) ---- *)

let hunt label ?cmd ?(port = 4000) ?(env = [||]) ?(timeout = 30.0) body =
  Eio_main.run @@ fun eio_env ->
  Eio.Switch.run @@ fun sw ->
  let net = (Eio.Stdenv.net eio_env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let clock = Eio.Stdenv.clock eio_env in
  let server_pid =
    match cmd with
    | None -> None
    | Some argv ->
      Array.iter (fun kv -> match String.index_opt kv '=' with Some i -> Unix.putenv (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1)) | None -> ()) env;
      let proc_mgr = Eio.Stdenv.process_mgr eio_env in
      let proc = Eio.Process.spawn ~sw proc_mgr argv in
      let pid = Eio.Process.pid proc in
      Test_server.wait_ready ~net ~clock ~port ~timeout;
      Some pid
  in
  let c = { port; net; last = None; server_pid; checks_passed = 0; checks_failed = 0 } in
  ctx := Some c;
  Printf.printf "\n\027[1m⟐ %s\027[0m \027[2m(:%d)\027[0m\n%!" label port;
  Fun.protect body ~finally:(fun () -> ctx := None);
  Printf.printf "\n";
  if c.checks_failed > 0 then (
    Printf.printf "  \027[31m%d/%d checks failed\027[0m\n%!" c.checks_failed (c.checks_passed + c.checks_failed);
    exit 1)
  else Printf.printf "  \027[32m%d checks passed\027[0m\n%!" c.checks_passed
