(* fennec_hunt.Http — a full-featured, deterministic HTTP testing layer.

   Every request is ONE TCP call → ONE response → immediate pass or fail. No polling, no
   retry, no timing dependencies in the test path. The only wait is the optional spawn
   readiness probe (one-time setup before any test runs).

   {[open Fennec_hunt.Http

     let () = hunt "my server" ~url:"http://localhost:4000" ~spawn:["./server"] @@ fun () ->

       check "health" (fun () ->
         get "/health" ~expect:[status 200; is_json; body_contains {|"ok":true|}]);

       check "auth flow" (fun () ->
         post "/login" ~body:{|{"user":"admin"}|} ~headers:[json_content_type]
           ~expect:[status 200];
         get "/dashboard" ~expect:[status 200; body_contains "Welcome"])
   ]} *)

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Types                                                                     *)
(* ════════════════════════════════════════════════════════════════════════════ *)

type response = Http_client.response = { status : int; headers : (string * string) list; body : string }
type assertion = response -> unit

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Ambient context                                                           *)
(* ════════════════════════════════════════════════════════════════════════════ *)

type url_parts = { scheme : string [@warning "-69"]; host : string [@warning "-69"]; port : int; base_path : string }

type context = {
  url : url_parts;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  mutable last : response option;
  mutable cookies : (string * string) list;
  mutable checks_passed : int;
  mutable checks_failed : int;
}

let preview s = if String.length s <= 200 then s else String.sub s 0 197 ^ "..."

let ctx : context option ref = ref None
let current () = match !ctx with Some c -> c | None -> failwith "fennec_hunt: not inside a `hunt` block"

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  URL parsing                                                               *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let parse_url url =
  let scheme, rest =
    if Fennec_hunt_util.contains url "://" then
      let i = String.index url ':' in (String.sub url 0 i, String.sub url (i + 3) (String.length url - i - 3))
    else ("http", url)
  in
  let host_port, base_path =
    match String.index_opt rest '/' with
    | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    | None -> (rest, "")
  in
  let host, port =
    match String.rindex_opt host_port ':' with
    | Some i ->
      let h = String.sub host_port 0 i in
      let p = try int_of_string (String.sub host_port (i + 1) (String.length host_port - i - 1)) with _ -> if scheme = "https" then 443 else 80 in
      (h, p)
    | None -> (host_port, if scheme = "https" then 443 else 80)
  in
  { scheme; host; port; base_path }

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Timing                                                                    *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let last_elapsed = ref 0.0

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Cookie jar (automatic, per-check)                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let parse_set_cookies (headers : (string * string) list) : (string * string) list =
  List.filter_map (fun (k, v) ->
      if String.lowercase_ascii k = "set-cookie" then
        match String.index_opt v '=' with
        | Some i ->
          let name = String.sub v 0 i in
          let rest = String.sub v (i + 1) (String.length v - i - 1) in
          let value = match String.index_opt rest ';' with Some j -> String.sub rest 0 j | None -> rest in
          Some (String.trim name, String.trim value)
        | None -> None
      else None)
    headers

let cookie_header jar =
  if jar = [] then None
  else Some ("Cookie", String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) jar))

let update_jar jar new_cookies =
  let updated = List.fold_left (fun acc (k, v) -> (k, v) :: List.filter (fun (k2, _) -> k2 <> k) acc) jar new_cookies in
  updated

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Assertion constructors (response -> unit)                                 *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let status code (r : response) =
  if r.status <> code then failwith (Printf.sprintf "expected status %d, got %d\n  body: %s" code r.status (preview r.body))

let status_2xx (r : response) =
  if r.status < 200 || r.status > 299 then failwith (Printf.sprintf "expected 2xx, got %d" r.status)

let status_3xx (r : response) =
  if r.status < 300 || r.status > 399 then failwith (Printf.sprintf "expected 3xx, got %d" r.status)

let status_4xx (r : response) =
  if r.status < 400 || r.status > 499 then failwith (Printf.sprintf "expected 4xx, got %d" r.status)

let status_5xx (r : response) =
  if r.status < 500 || r.status > 599 then failwith (Printf.sprintf "expected 5xx, got %d" r.status)

let body_contains needle (r : response) =
  if not (Fennec_hunt_util.contains r.body needle) then
    failwith (Printf.sprintf "body does not contain %S\n  body: %s" needle (preview r.body))

let body_is expected (r : response) =
  if r.body <> expected then failwith (Printf.sprintf "body mismatch\n  expected: %s\n  got:      %s" (preview expected) (preview r.body))

let body_not_contains needle (r : response) =
  if Fennec_hunt_util.contains r.body needle then
    failwith (Printf.sprintf "body should NOT contain %S but does\n  body: %s" needle (preview r.body))

let header_is name expected (r : response) =
  match Http_client.header_value name r with
  | Some v when v = expected -> ()
  | Some v -> failwith (Printf.sprintf "header %S: expected %S, got %S" name expected v)
  | None -> failwith (Printf.sprintf "header %S: expected %S, but absent" name expected)

let header_contains name needle (r : response) =
  match Http_client.header_value name r with
  | Some v when Fennec_hunt_util.contains v needle -> ()
  | Some v -> failwith (Printf.sprintf "header %S: expected to contain %S, got %S" name needle v)
  | None -> failwith (Printf.sprintf "header %S: expected to contain %S, but absent" name needle)

let has_header name (r : response) =
  if Http_client.header_value name r = None then failwith (Printf.sprintf "expected header %S, but absent" name)

let no_header name (r : response) =
  match Http_client.header_value name r with
  | Some v -> failwith (Printf.sprintf "expected NO header %S, but found %S" name v) | None -> ()

let content_type expected = header_contains "content-type" expected
let is_json = content_type "json"
let is_html = content_type "html"

let redirect_to target (r : response) =
  if r.status < 300 || r.status > 399 then failwith (Printf.sprintf "expected redirect, got %d" r.status);
  match Http_client.header_value "location" r with
  | Some loc when Fennec_hunt_util.contains loc target -> ()
  | Some loc -> failwith (Printf.sprintf "redirect location: expected to contain %S, got %S" target loc)
  | None -> failwith "expected redirect with Location header, but Location absent"

let max_elapsed ms (r : response) =
  ignore r;
  if !last_elapsed > ms then failwith (Printf.sprintf "response took %.0fms, limit %.0fms" !last_elapsed ms)

let min_body_length n (r : response) =
  let actual = String.length r.body in
  if actual < n then failwith (Printf.sprintf "body length %d, expected >= %d" actual n)

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Request functions                                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let run_expect r = function None -> () | Some checks -> List.iter (fun (a : assertion) -> a r) checks

let request meth ?(headers = []) ?host ?body ?(expect : assertion list option) path =
  let c = current () in
  let full_path = c.url.base_path ^ path in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let headers = match cookie_header c.cookies with Some ch -> ch :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.request ~net:c.net ~port:c.url.port ~meth ~path:full_path ~headers ?body () in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  c.cookies <- update_jar c.cookies (parse_set_cookies r.headers);
  run_expect r expect

let get ?headers ?host ?expect path = request "GET" ?headers ?host ?expect path
let post ?headers ?host ?body ?expect path = request "POST" ?headers ?host ?body ?expect path
let put ?headers ?host ?body ?expect path = request "PUT" ?headers ?host ?body ?expect path
let patch ?headers ?host ?body ?expect path = request "PATCH" ?headers ?host ?body ?expect path
let delete ?headers ?host ?expect path = request "DELETE" ?headers ?host ?expect path
let head ?headers ?host ?expect path = request "HEAD" ?headers ?host ?expect path
let options ?headers ?host ?expect path = request "OPTIONS" ?headers ?host ?expect path

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Extractors (from last response)                                           *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let last () = match (current ()).last with Some r -> r | None -> failwith "no response yet"

let header name =
  match Http_client.header_value name (last ()) with Some v -> v | None -> failwith (Printf.sprintf "header %S absent" name)

let header_opt name = Http_client.header_value name (last ())
let response_body () = (last ()).body
let response_status () = (last ()).status
let elapsed_ms () = !last_elapsed

let json_field key =
  let body = (last ()).body in
  try
    match Yojson.Safe.from_string body with
    | `Assoc pairs -> (
      match List.assoc_opt key pairs with
      | Some (`String s) -> s
      | Some v -> Yojson.Safe.to_string v
      | None -> failwith (Printf.sprintf "JSON field %S not found" key))
    | _ -> failwith "response body is not a JSON object"
  with Yojson.Json_error msg -> failwith (Printf.sprintf "invalid JSON: %s\n  body: %s" msg (preview body))

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Helpers                                                                   *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let basic_auth user pass =
  ("Authorization", "Basic " ^ Base64.encode_string (user ^ ":" ^ pass))

let bearer token = ("Authorization", "Bearer " ^ token)
let json_content_type = ("Content-Type", "application/json")

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  check                                                                     *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let check label body =
  let c = current () in
  c.last <- None;
  c.cookies <- [];
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

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  hunt                                                                      *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let hunt label ~url ?spawn ?(env = [||]) ?(timeout = 30.0) body =
  let url_parts = parse_url url in
  Eio_main.run @@ fun eio_env ->
  Eio.Switch.run @@ fun sw ->
  let net = (Eio.Stdenv.net eio_env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let clock = Eio.Stdenv.clock eio_env in
  (match spawn with
  | None -> ()
  | Some argv ->
    Array.iter (fun kv -> match String.index_opt kv '=' with Some i -> Unix.putenv (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1)) | None -> ()) env;
    let proc_mgr = Eio.Stdenv.process_mgr eio_env in
    let devnull = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) Eio.Path.(Eio.Stdenv.fs eio_env / "/dev/null") in
    ignore (Eio.Process.spawn ~sw proc_mgr ~stdout:devnull ~stderr:devnull argv);
    Test_server.wait_ready ~net ~clock ~port:url_parts.port ~timeout);
  let c = { url = url_parts; net; last = None; cookies = []; checks_passed = 0; checks_failed = 0 } in
  ctx := Some c;
  Printf.printf "\n\027[1m⟐ %s\027[0m \027[2m(%s)\027[0m\n%!" label url;
  Fun.protect body ~finally:(fun () -> ctx := None);
  Printf.printf "\n";
  if c.checks_failed > 0 then (
    Printf.printf "  \027[31m%d/%d checks failed\027[0m\n%!" c.checks_failed (c.checks_passed + c.checks_failed);
    exit 1)
  else Printf.printf "  \027[32m%d checks passed\027[0m\n%!" c.checks_passed
