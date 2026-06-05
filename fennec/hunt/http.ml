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

let body_length n (r : response) =
  let actual = String.length r.body in
  if actual <> n then failwith (Printf.sprintf "body length: expected %d, got %d" n actual)

(* regex match on the body *)
let body_matches pattern (r : response) =
  let re = Re.Pcre.re pattern |> Re.compile in
  if not (Re.execp re r.body) then
    failwith (Printf.sprintf "body does not match /%s/\n  body: %s" pattern (preview r.body))

(* custom assertion — the escape hatch for anything we didn't think of *)
let expect f (r : response) = f r

(* ---- JSON assertions (nested path support) ---- *)

let parse_json (r : response) =
  try Yojson.Safe.from_string r.body
  with Yojson.Json_error msg -> failwith (Printf.sprintf "invalid JSON: %s\n  body: %s" msg (preview r.body))

let rec json_walk (j : Yojson.Safe.t) = function
  | [] -> Some j
  | key :: rest -> (
    match j with
    | `Assoc pairs -> ( match List.assoc_opt key pairs with Some v -> json_walk v rest | None -> None)
    | _ -> None)

let json_path_value path (r : response) : Yojson.Safe.t option =
  let keys = String.split_on_char '.' path in
  json_walk (parse_json r) keys

(* stringify a JSON value for comparison: strings unwrap, everything else is JSON notation *)
let json_to_comparable = function `String s -> s | v -> Yojson.Safe.to_string v

(* assert a dotted JSON path equals a value (compared as strings — "42" matches both the
   string "42" and the number 42; "true" matches the boolean true) *)
let json_path_is path expected (r : response) =
  match json_path_value path r with
  | Some v when json_to_comparable v = expected -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected %S, got %s" path expected (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found\n  body: %s" path (preview r.body))

(* assert a dotted JSON path contains a substring *)
let json_path_contains path needle (r : response) =
  match json_path_value path r with
  | Some (`String s) when Fennec_hunt_util.contains s needle -> ()
  | Some (`String s) -> failwith (Printf.sprintf "JSON %s: expected to contain %S, got %S" path needle s)
  | Some v -> failwith (Printf.sprintf "JSON %s: not a string (%s)" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

(* assert a dotted JSON path exists (any value) *)
let json_has path (r : response) =
  match json_path_value path r with
  | Some _ -> ()
  | None -> failwith (Printf.sprintf "JSON path %S not found\n  body: %s" path (preview r.body))

(* assert a JSON array at the path has N elements *)
let json_length path n (r : response) =
  match json_path_value path r with
  | Some (`List l) ->
    let actual = List.length l in
    if actual <> n then failwith (Printf.sprintf "JSON %s: expected %d elements, got %d" path n actual)
  | Some v -> failwith (Printf.sprintf "JSON %s: not an array (%s)" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

(* assert a dotted JSON path is a specific type *)
let json_is_string path (r : response) =
  match json_path_value path r with
  | Some (`String _) -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected string, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let json_is_number path (r : response) =
  match json_path_value path r with
  | Some (`Int _ | `Float _) -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected number, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let json_is_bool path (r : response) =
  match json_path_value path r with
  | Some (`Bool _) -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected bool, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let json_is_null path (r : response) =
  match json_path_value path r with
  | Some `Null -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected null, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let json_is_array path (r : response) =
  match json_path_value path r with
  | Some (`List _) -> ()
  | Some v -> failwith (Printf.sprintf "JSON %s: expected array, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

(* regex match on a JSON string field *)
let json_path_matches path pattern (r : response) =
  let re = Re.Pcre.re pattern |> Re.compile in
  match json_path_value path r with
  | Some (`String s) when Re.execp re s -> ()
  | Some (`String s) -> failwith (Printf.sprintf "JSON %s: %S does not match /%s/" path s pattern)
  | Some v -> failwith (Printf.sprintf "JSON %s: not a string (%s)" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let uuid_re = Re.Pcre.re "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" |> Re.compile
let json_is_uuid path (r : response) =
  match json_path_value path r with
  | Some (`String s) when Re.execp uuid_re (String.lowercase_ascii s) -> ()
  | Some (`String s) -> failwith (Printf.sprintf "JSON %s: %S is not a UUID" path s)
  | Some v -> failwith (Printf.sprintf "JSON %s: expected UUID string, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

let iso_date_re = Re.Pcre.re "^\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2})" |> Re.compile
let json_is_datetime path (r : response) =
  match json_path_value path r with
  | Some (`String s) when Re.execp iso_date_re s -> ()
  | Some (`String s) -> failwith (Printf.sprintf "JSON %s: %S is not an ISO datetime" path s)
  | Some v -> failwith (Printf.sprintf "JSON %s: expected datetime string, got %s" path (Yojson.Safe.to_string v))
  | None -> failwith (Printf.sprintf "JSON path %S not found" path)

(* body emptiness *)
let body_empty (r : response) =
  if r.body <> "" then failwith (Printf.sprintf "expected empty body, got %d bytes: %s" (String.length r.body) (preview r.body))

let body_not_empty (r : response) =
  if r.body = "" then failwith "expected non-empty body, got empty"

(* status negation *)
let status_not code (r : response) =
  if r.status = code then failwith (Printf.sprintf "expected any status except %d, got %d" code r.status)

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  URL + body encoding helpers                                               *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let url_encode s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> Buffer.add_char buf c
      | _ -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

let encode_query pairs =
  String.concat "&" (List.map (fun (k, v) -> url_encode k ^ "=" ^ url_encode v) pairs)

let encode_form pairs =
  (encode_query pairs, "application/x-www-form-urlencoded")

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Request functions                                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let run_expect r = function
  | None -> ()
  | Some checks ->
    try List.iter (fun (a : assertion) -> a r) checks
    with Failure msg ->
      let path = match (current ()).url.base_path with "" -> "" | p -> p in
      failwith (Printf.sprintf "%s\n  elapsed: %.0fms\n  url: %s" msg !last_elapsed path)

let request meth ?(headers = []) ?host ?body ?query ?form ?json ?(expect : assertion list option) path =
  let c = current () in
  (* query parameters *)
  let full_path =
    let base = c.url.base_path ^ path in
    match query with None -> base | Some pairs -> base ^ "?" ^ encode_query pairs
  in
  (* body: ~json > ~form > ~body (first one wins), and set Content-Type automatically *)
  let body, headers =
    match json with
    | Some j ->
      let json_str = Yojson.Safe.to_string j in
      (Some json_str, ("Content-Type", "application/json") :: headers)
    | None -> (
      match form with
      | Some pairs ->
        let encoded, ct = encode_form pairs in
        (Some encoded, ("Content-Type", ct) :: headers)
      | None -> (body, headers))
  in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let headers = match cookie_header c.cookies with Some ch -> ch :: headers | None -> headers in
  let t0 = Unix.gettimeofday () in
  let r = Http_client.request ~net:c.net ~port:c.url.port ~meth ~path:full_path ~headers ?body () in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some r;
  c.cookies <- update_jar c.cookies (parse_set_cookies r.headers);
  run_expect r expect

let get ?headers ?host ?query ?expect path = request "GET" ?headers ?host ?query ?expect path
let post ?headers ?host ?body ?query ?form ?json ?expect path = request "POST" ?headers ?host ?body ?query ?form ?json ?expect path
let put ?headers ?host ?body ?query ?form ?json ?expect path = request "PUT" ?headers ?host ?body ?query ?form ?json ?expect path
let patch ?headers ?host ?body ?query ?form ?json ?expect path = request "PATCH" ?headers ?host ?body ?query ?form ?json ?expect path
let delete ?headers ?host ?query ?expect path = request "DELETE" ?headers ?host ?query ?expect path
let head ?headers ?host ?query ?expect path = request "HEAD" ?headers ?host ?query ?expect path
let options ?headers ?host ?query ?expect path = request "OPTIONS" ?headers ?host ?query ?expect path

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

(* extract a top-level JSON field *)
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

(* extract a nested JSON field via dotted path *)
let json_path path =
  let r = last () in
  match json_path_value path r with
  | Some (`String s) -> s
  | Some v -> Yojson.Safe.to_string v
  | None -> failwith (Printf.sprintf "JSON path %S not found\n  body: %s" path (preview r.body))

(* get the full parsed JSON body *)
let json () = parse_json (last ())

(* cookie inspection *)
let cookie name =
  let c = current () in
  match List.assoc_opt name c.cookies with
  | Some v -> v
  | None -> failwith (Printf.sprintf "cookie %S not in jar (have: %s)" name
      (String.concat ", " (List.map fst c.cookies)))

let cookie_opt name = List.assoc_opt name (current ()).cookies

let has_cookie name (r : response) =
  let cookies = parse_set_cookies r.headers in
  if not (List.exists (fun (k, _) -> k = name) cookies) then
    failwith (Printf.sprintf "expected Set-Cookie %S, but not set" name)

let no_cookie name (r : response) =
  let cookies = parse_set_cookies r.headers in
  if List.exists (fun (k, _) -> k = name) cookies then
    failwith (Printf.sprintf "expected NO Set-Cookie %S, but found it" name)

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
