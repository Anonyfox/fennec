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

(* the server under test — its URL (parsed once) and the net capability to reach it. The
   ambient context every bare function reads; set by [hunt], cleared on exit. *)
type context = {
  url : Target.url;
  net : Target.net;
  clock : Target.clock; (* for per-request timeouts + the eventually poll *)
  request_timeout : float; (* default per-request deadline, seconds; overridable per call *)
  mutable last : response option;
  mutable last_request : string; (* "GET /api/health" — the request behind [last], for diagnostics *)
  mutable cookies : (string * string) list;
  mutable checks_passed : int;
  mutable checks_failed : int;
  mutable checks_skipped : int; (* filtered out by --grep — reported, never silently dropped *)
}

let preview s = if String.length s <= 200 then s else String.sub s 0 197 ^ "..."

let ctx : context option ref = ref None

(* every request/assertion/extractor reads the ambient context. It is [None] only when called
   outside a [hunt] block — a usage error, reported clearly (never a silent miscompute). *)
let current () =
  match !ctx with
  | Some c -> c
  | None -> failwith "fennec_hunt: a request/assertion was used outside a `hunt` block — wrap it in `hunt \"…\" ~url:\"…\" @@ fun () -> …`"

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Output styling — reuse the Browser layer's capability detection so colour    *)
(*  degrades on a non-TTY / NO_COLOR / dumb terminal exactly the same way        *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let caps = lazy (Reporter.detect_caps ())
let color code s = if (Lazy.force caps).Reporter.color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let glyph uni ascii = if (Lazy.force caps).Reporter.unicode then uni else ascii

(* Any failed check across ALL hunt blocks in the process. A hunt never exits mid-run, so
   every suite reports; the process exits non-zero once at the end if anything failed. *)
let any_failed = ref false
let exit_hook_installed = ref false
let exiting = ref false
let install_exit_hook () =
  if not !exit_hook_installed then begin
    exit_hook_installed := true;
    at_exit (fun () -> if !any_failed && not !exiting then (exiting := true; exit 1))
  end

(* the --grep filter, read once from argv (mirrors the Browser runner's flag) — set per-suite
   by `fennec test --grep`. A check whose label doesn't contain it is skipped (substring match,
   same semantics as the Browser cut and [body_contains]). *)
let grep =
  lazy
    (let rec scan = function
       | "--grep" :: v :: _ -> Some v
       | _ :: r -> scan r
       | [] -> None
     in
     match Array.to_list Sys.argv with _ :: rest -> scan rest | [] -> None)

let selected label = match Lazy.force grep with None -> true | Some g -> Fennec_hunt_util.contains label g

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

(* ---- multipart/form-data (file uploads) ---- *)

(* a single part: a plain text field, or a file (with a filename + content-type) *)
type part = { p_name : string; p_filename : string option; p_ctype : string option; p_content : string }

let field name value = { p_name = name; p_filename = None; p_ctype = None; p_content = value }
let file ~name ~filename ?content_type content =
  { p_name = name; p_filename = Some filename; p_ctype = content_type; p_content = content }

(* a fixed boundary: a testing client controls both ends, and a collision with content is
   astronomically unlikely. Keeping it fixed makes the encoding deterministic (and testable). *)
let multipart_boundary = "----FennecHuntBoundaryZ9xK3mQ7vP1nT0"

let encode_multipart ~boundary (parts : part list) : string =
  let b = Buffer.create 256 in
  List.iter
    (fun p ->
      Buffer.add_string b ("--" ^ boundary ^ "\r\n");
      Buffer.add_string b (Printf.sprintf "Content-Disposition: form-data; name=\"%s\"" p.p_name);
      (match p.p_filename with Some fn -> Buffer.add_string b (Printf.sprintf "; filename=\"%s\"" fn) | None -> ());
      Buffer.add_string b "\r\n";
      (match p.p_ctype with Some ct -> Buffer.add_string b ("Content-Type: " ^ ct ^ "\r\n") | None -> ());
      Buffer.add_string b "\r\n";
      Buffer.add_string b p.p_content;
      Buffer.add_string b "\r\n")
    parts;
  Buffer.add_string b ("--" ^ boundary ^ "--\r\n");
  Buffer.contents b

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Request functions                                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let run_expect r = function
  | None -> ()
  | Some checks ->
    try List.iter (fun (a : assertion) -> a r) checks
    with Failure msg ->
      failwith (Printf.sprintf "%s\n  request: %s\n  elapsed: %.0fms" msg (current ()).last_request !last_elapsed)

(* pure redirect-following policy: from [first], while [location] yields a hop, [fetch] it,
   up to [max] hops. Generic over the response type so it's unit-testable with a fake fetch. *)
let follow_redirects ~max ~location ~fetch first =
  let rec go hops resp =
    if hops >= max then resp
    else match location resp with Some loc -> go (hops + 1) (fetch loc) | None -> resp
  in
  go 0 first

(* resolve a Location header to a path on the current target. Absolute URLs keep only their
   path (a testing client follows same-target; cross-host hops aren't chased to other hosts). *)
let redirect_path loc =
  if loc = "" then "/"
  else if loc.[0] = '/' then loc
  else if Fennec_hunt_util.contains loc "://" then (match (Target.parse_url loc).base_path with "" -> "/" | p -> p)
  else "/" ^ loc

let request meth ?(headers = []) ?host ?body ?query ?form ?json ?multipart ?(follow = false) ?timeout ?(expect : assertion list option) path =
  let c = current () in
  let timeout = Option.value timeout ~default:c.request_timeout in
  let full_path =
    let base = c.url.base_path ^ path in
    match query with None -> base | Some pairs -> base ^ "?" ^ encode_query pairs
  in
  (* body: ~json > ~multipart > ~form > ~body (first one wins); Content-Type set automatically *)
  let body, ct_header =
    match json with
    | Some j -> (Some (Yojson.Safe.to_string j), [ ("Content-Type", "application/json") ])
    | None -> (
      match multipart with
      | Some parts -> (Some (encode_multipart ~boundary:multipart_boundary parts), [ ("Content-Type", "multipart/form-data; boundary=" ^ multipart_boundary) ])
      | None -> (
        match form with
        | Some pairs -> let encoded, ct = encode_form pairs in (Some encoded, [ ("Content-Type", ct) ])
        | None -> (body, [])))
  in
  (* [~host] overrides the Host HEADER (virtual-host testing); the CONNECTION still goes to the
     target's real host:port. User headers + host persist across redirect hops; the Content-Type
     applies only to the body-bearing initial request. *)
  let host_header = match host with Some h -> [ ("Host", h) ] | None -> [] in
  (* one round-trip, timeout-bounded, refreshing the cookie jar from the response *)
  let send meth path body extra =
    let hdrs = (match cookie_header c.cookies with Some ch -> [ ch ] | None -> []) @ host_header @ extra in
    let r =
      match Eio.Time.with_timeout c.clock timeout (fun () ->
          Ok (Http_client.request ~net:c.net ~host:c.url.host ~port:c.url.port ~tls:(c.url.scheme = "https") ~meth ~path ~headers:hdrs ?body ()))
      with
      | Ok r -> r
      | Error `Timeout -> failwith (Printf.sprintf "request timed out after %.1fs: %s %s" timeout meth path)
    in
    c.cookies <- update_jar c.cookies (parse_set_cookies r.headers);
    r
  in
  c.last_request <- Printf.sprintf "%s %s" meth full_path;
  let t0 = Unix.gettimeofday () in
  (* the initial request; then, if [~follow], chase 3xx Location hops (re-GET with refreshed
     cookies — so a post-login redirect carries its session). Bounded to 10 hops. *)
  let r0 = send meth full_path body (ct_header @ headers) in
  let final =
    if not follow then r0
    else
      let location resp =
        if resp.Http_client.status >= 300 && resp.status < 400 then Option.map redirect_path (Http_client.header_value "location" resp)
        else None
      in
      follow_redirects ~max:10 ~location ~fetch:(fun loc -> send "GET" loc None headers) r0
  in
  last_elapsed := (Unix.gettimeofday () -. t0) *. 1000.0;
  c.last <- Some final;
  run_expect final expect

let get ?headers ?host ?query ?follow ?timeout ?expect path = request "GET" ?headers ?host ?query ?follow ?timeout ?expect path
let post ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path = request "POST" ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path
let put ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path = request "PUT" ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path
let patch ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path = request "PATCH" ?headers ?host ?body ?query ?form ?json ?multipart ?follow ?timeout ?expect path
let delete ?headers ?host ?query ?follow ?timeout ?expect path = request "DELETE" ?headers ?host ?query ?follow ?timeout ?expect path
let head ?headers ?host ?query ?follow ?timeout ?expect path = request "HEAD" ?headers ?host ?query ?follow ?timeout ?expect path
let options ?headers ?host ?query ?follow ?timeout ?expect path = request "OPTIONS" ?headers ?host ?query ?follow ?timeout ?expect path

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  eventually — explicit, bounded polling for async expectations              *)
(* ════════════════════════════════════════════════════════════════════════════ *)

(* the pure policy: re-run [body] until it stops raising (assertions pass) or the deadline.
   [now]/[sleep] are injected so this is deterministically unit-testable. NOT flaky-retry —
   the caller opts in explicitly for a genuinely async expectation. *)
let poll ~now ~sleep ~within ~interval (body : unit -> unit) : unit =
  let deadline = now () +. within in
  let rec loop () =
    match body () with
    | () -> ()
    | exception Failure msg ->
      if now () >= deadline then
        failwith (Printf.sprintf "eventually: condition not met within %.1fs\n  last failure: %s" within msg)
      else (sleep interval; loop ())
  in
  loop ()

let eventually ?(within = 5.0) ?(interval = 0.2) body =
  let c = current () in
  poll ~now:(fun () -> Eio.Time.now c.clock) ~sleep:(Eio.Time.sleep c.clock) ~within ~interval body

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
  if not (selected label) then (
    c.checks_skipped <- c.checks_skipped + 1;
    Printf.printf "  %s  %s\n%!" (color "2" (glyph "–" "-")) (color "2" (label ^ " (skipped by --grep)")))
  else begin
  c.last <- None;
  c.cookies <- [];
  let t0 = Unix.gettimeofday () in
  match body () with
  | () ->
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    c.checks_passed <- c.checks_passed + 1;
    Printf.printf "  %s  %s %s\n%!" (color "32" (glyph "✓" "ok")) label (color "2" (Printf.sprintf "(%.0fms)" ms))
  | exception e ->
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    c.checks_failed <- c.checks_failed + 1;
    Printf.printf "  %s  %s %s\n%!" (color "31" (glyph "✗" "FAIL")) label (color "2" (Printf.sprintf "(%.0fms)" ms));
    (* render the message RAW and multi-line — an assertion failure (Failure msg) carries a
       formatted multi-line diagnostic; Printexc.to_string would escape the newlines onto one
       line and wrap it in Failure("…"). Other exceptions fall back to their string form. *)
    let detail = match e with Failure m -> m | other -> Printexc.to_string other in
    String.split_on_char '\n' detail |> List.iter (fun line -> Printf.printf "     %s\n%!" line)
  end

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  hunt                                                                      *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let hunt label ?url ?spawn ?(env = [||]) ?(timeout = 30.0) ?(request_timeout = 10.0) body =
  (* the target instance: explicit ~url, else the harness-assigned FENNEC_TEST_URL (so a suite
     run by `fennec test` can't hardcode a colliding port — it gets its isolated instance),
     else a clear error. *)
  let url = match Test_proto.resolve_url ~explicit:url with Ok u -> u | Error m -> failwith ("fennec_hunt: " ^ m) in
  let target = Target.parse_url url in
  let tls = target.scheme = "https" in
  Eio_main.run @@ fun eio_env ->
  Eio.Switch.run @@ fun sw ->
  let net = (Eio.Stdenv.net eio_env :> Target.net) in
  let clock = (Eio.Stdenv.clock eio_env :> Target.clock) in
  (match spawn with
  | None ->
    (* no spawn: an already-running server. Still wait for it (it may be coming up). *)
    Target.wait_ready ~net ~clock ~host:target.host ~port:target.port ~tls ~timeout ()
  | Some argv ->
    Target.spawn ~sw ~proc_mgr:(Eio.Stdenv.process_mgr eio_env) ~fs:(Eio.Stdenv.fs eio_env)
      ~net ~clock ~env ~host:target.host ~port:target.port ~tls ~timeout argv);
  let c = { url = target; net; clock; request_timeout; last = None; last_request = "(no request yet)"; cookies = []; checks_passed = 0; checks_failed = 0; checks_skipped = 0 } in
  install_exit_hook ();
  ctx := Some c;
  Printf.printf "\n%s %s\n%!" (color "1" (glyph "⟐ " "> " ^ label)) (color "2" (Printf.sprintf "(%s)" url));
  Fun.protect body ~finally:(fun () -> ctx := None);
  Printf.printf "\n";
  (* a hunt never exits here — it records failure and lets later suites in the same process
     run. The process exits non-zero once, at the end, via the at_exit hook. *)
  let skipped = if c.checks_skipped > 0 then Printf.sprintf " (%d skipped)" c.checks_skipped else "" in
  if c.checks_failed > 0 then (
    any_failed := true;
    Printf.printf "  %s%s\n%!" (color "31" (Printf.sprintf "%d/%d checks failed" c.checks_failed (c.checks_passed + c.checks_failed))) (color "2" skipped))
  else Printf.printf "  %s%s\n%!" (color "32" (Printf.sprintf "%d checks passed" c.checks_passed)) (color "2" skipped)

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  For_test — pure internals exposed for unit tests; NOT a stable API           *)
(* ════════════════════════════════════════════════════════════════════════════ *)

module For_test = struct
  let poll = poll
  let decode_chunked = Http_client.decode_chunked
  let encode_multipart = encode_multipart
  let follow_redirects = follow_redirects
  let redirect_path = redirect_path
  (* parse_url as a tuple so the test needn't see Target.url *)
  let parse_url s = let u = Target.parse_url s in (u.Target.scheme, u.host, u.port, u.base_path)
  let encode_query = encode_query
  let encode_form = encode_form
  let parse_set_cookies = parse_set_cookies
  let update_jar = update_jar
end

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Inline tests (For_test pure cores + assertion surface)                     *)
(* ════════════════════════════════════════════════════════════════════════════ *)

(* --- poll (deterministic fake clock) --- *)
let%test_unit "poll: passes on first try" =
  let t = ref 0.0 in let now () = !t in let sleep dt = t := !t +. dt in
  let calls = ref 0 in
  For_test.poll ~now ~sleep ~within:5.0 ~interval:0.2 (fun () -> incr calls);
  Fennec_hunt_unit.check "1 call" (!calls = 1)

let%test_unit "poll: passes on 3rd try" =
  let t = ref 0.0 in let now () = !t in let sleep dt = t := !t +. dt in
  let calls = ref 0 in
  For_test.poll ~now ~sleep ~within:5.0 ~interval:0.2 (fun () -> incr calls; if !calls < 3 then failwith "not yet");
  Fennec_hunt_unit.check "3 calls" (!calls = 3)

let%test "poll: always-failing raises after deadline" =
  let t = ref 0.0 in let now () = !t in let sleep dt = t := !t +. dt in
  try For_test.poll ~now ~sleep ~within:1.0 ~interval:0.2 (fun () -> failwith "still"); false
  with Failure m -> Fennec_hunt_util.contains m "still" && Fennec_hunt_util.contains m "within 1.0s"

(* --- decode_chunked --- *)
let%test "chunked: two chunks joined"   = For_test.decode_chunked "2\r\nab\r\n2\r\ncd\r\n0\r\n\r\n" = "abcd"
let%test "chunked: single chunk"        = For_test.decode_chunked "5\r\nhello\r\n0\r\n\r\n" = "hello"
let%test "chunked: hex size 0x10"       = For_test.decode_chunked "10\r\n0123456789abcdef\r\n0\r\n\r\n" = "0123456789abcdef"
let%test "chunked: extension ignored"   = For_test.decode_chunked "2;foo=bar\r\nok\r\n0\r\n\r\n" = "ok"
let%test "chunked: empty"               = For_test.decode_chunked "0\r\n\r\n" = ""
let%test "chunked: malformed best-effort" = For_test.decode_chunked "2\r\nab\r\nGARBAGE" = "ab"

(* --- encode_multipart --- *)
let%test "multipart: field part" =
  let mp = For_test.encode_multipart ~boundary:"B" [ field "t" "hi" ] in
  Fennec_hunt_util.contains mp "name=\"t\"\r\n\r\nhi\r\n"
let%test "multipart: file part" =
  let mp = For_test.encode_multipart ~boundary:"B" [ file ~name:"f" ~filename:"a.txt" ~content_type:"text/plain" "D" ] in
  Fennec_hunt_util.contains mp "filename=\"a.txt\"" && Fennec_hunt_util.contains mp "Content-Type: text/plain"
let%test "multipart: closing boundary" =
  Fennec_hunt_util.contains (For_test.encode_multipart ~boundary:"B" [ field "x" "y" ]) "--B--\r\n"

(* --- follow_redirects --- *)
let%test "redirects: follows chain to 200" =
  let chain = [ "/b", (302, Some "/c"); "/c", (200, None) ] in
  let location (s, l) = if s >= 300 && s < 400 then l else None in
  let fetch l = try List.assoc l chain with Not_found -> (599, None) in
  For_test.follow_redirects ~max:10 ~location ~fetch (302, Some "/b") = (200, None)
let%test "redirects: no redirect passes through" =
  For_test.follow_redirects ~max:10 ~location:(fun _ -> None) ~fetch:(fun _ -> (200, None)) (200, None) = (200, None)
let%test "redirects: cycle bounded by max" =
  let cyclic _ = (302, Some "/loop") in
  For_test.follow_redirects ~max:3 ~location:(fun (s,l) -> if s=302 then l else None) ~fetch:cyclic (302, Some "/loop") = (302, Some "/loop")

(* --- redirect_path --- *)
let%test "redirect_path: absolute path"   = For_test.redirect_path "/dash?x=1" = "/dash?x=1"
let%test "redirect_path: absolute URL"    = For_test.redirect_path "http://h:8080/dash?x=1" = "/dash?x=1"
let%test "redirect_path: URL no path"     = For_test.redirect_path "http://host" = "/"
let%test "redirect_path: relative"        = For_test.redirect_path "dash" = "/dash"

(* --- parse_url --- *)
let%test "parse_url: full"           = For_test.parse_url "http://localhost:4000/api/x" = ("http", "localhost", 4000, "/api/x")
let%test "parse_url: no scheme"      = For_test.parse_url "example.com:8080" = ("http", "example.com", 8080, "")
let%test "parse_url: https default"  = For_test.parse_url "https://acme.com" = ("https", "acme.com", 443, "")
let%test "parse_url: bare host"      = For_test.parse_url "localhost" = ("http", "localhost", 80, "")

(* --- encoders --- *)
let%test "encode_query escapes"      = For_test.encode_query [("q", "a b&c"); ("n", "1")] = "q=a%20b%26c&n=1"
let%test "encode_form content-type"  = snd (For_test.encode_form [("a", "1")]) = "application/x-www-form-urlencoded"
let%test "encode_form body"          = fst (For_test.encode_form [("a", "x y")]) = "a=x%20y"

(* --- cookie jar --- *)
let%test "parse_set_cookies strips attrs" = For_test.parse_set_cookies [("Set-Cookie", "sid=abc; Path=/; HttpOnly")] = [("sid", "abc")]
let%test "parse_set_cookies ci header"    = For_test.parse_set_cookies [("set-cookie", "a=1")] = [("a", "1")]
let%test "update_jar adds"                = List.sort compare (For_test.update_jar [("a","1")] [("b","2")]) = [("a","1"); ("b","2")]
let%test "update_jar overwrites"          = For_test.update_jar [("a","1")] [("a","2")] = [("a","2")]

(* --- helpers --- *)
let%test "basic_auth header"        = basic_auth "user" "pass" = ("Authorization", "Basic dXNlcjpwYXNz")
let%test "bearer header"            = bearer "tok" = ("Authorization", "Bearer tok")
let%test "json_content_type"        = json_content_type = ("Content-Type", "application/json")

(* --- assertion surface (against constructed responses) --- *)
let%test_unit "assertions: status" =
  let r ok = { status = ok; headers = []; body = "" } in
  status 200 (r 200); status_2xx (r 201); status_3xx (r 302); status_4xx (r 404); status_5xx (r 503);
  (match (try status 200 (r 404); false with _ -> true) with true -> () | false -> failwith "status 200 on 404 should fail")

let%test_unit "assertions: body" =
  let r b = { status = 200; headers = []; body = b } in
  body_contains "ell" (r "hello"); body_is "hi" (r "hi"); body_not_contains "z" (r "hi");
  body_empty (r ""); body_not_empty (r "x"); body_length 5 (r "hello"); min_body_length 3 (r "hello");
  body_matches "^[0-9]+$" (r "42")

let%test_unit "assertions: headers" =
  let r h = { status = 200; headers = h; body = "" } in
  header_is "X-A" "1" (r [("X-A","1")]); header_contains "content-type" "json" (r [("Content-Type","application/json")]);
  has_header "X-A" (r [("X-A","1")]); no_header "X-Z" (r []); is_json (r [("Content-Type","application/json")]);
  is_html (r [("Content-Type","text/html; charset=utf-8")]); content_type "json" (r [("Content-Type","application/json")])

let%test_unit "assertions: JSON paths" =
  let jbody = {|{"name":"alice","id":"550e8400-e29b-41d4-a716-446655440000","age":30,"tags":["a","b"],"when":"2026-01-02T03:04:05Z","ok":true,"maybe":null}|} in
  let r = { status = 200; headers = []; body = jbody } in
  json_path_is "name" "alice" r; json_path_is "age" "30" r; json_path_is "ok" "true" r;
  json_has "id" r; json_length "tags" 2 r; json_is_uuid "id" r; json_is_datetime "when" r;
  json_is_number "age" r; json_is_array "tags" r; json_is_string "name" r; json_is_bool "ok" r;
  json_is_null "maybe" r; json_path_contains "name" "lic" r; json_path_matches "id" "^[0-9a-f-]+$" r

let%test_unit "assertions: redirect + cookie" =
  redirect_to "/home" { status = 302; headers = [("Location","/home")]; body = "" };
  has_cookie "sid" { status = 200; headers = [("Set-Cookie","sid=abc; Path=/")]; body = "" };
  no_cookie "sid" { status = 200; headers = []; body = "" }

(* --- Test_proto.resolve (pure) --- *)
let%test "resolve: explicit wins"    = Test_proto.resolve ~explicit:(Some "http://a") ~from_env:(Some "http://b") = Ok "http://a"
let%test "resolve: env fallback"     = Test_proto.resolve ~explicit:None ~from_env:(Some "http://b") = Ok "http://b"
let%test "resolve: neither → error"  = match Test_proto.resolve ~explicit:None ~from_env:None with Error _ -> true | Ok _ -> false
let%test "url_for builds url"        = Test_proto.url_for ~port:7001 = "http://localhost:7001"
