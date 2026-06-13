module Challenge = Accounts_challenge
module Identity = Accounts_identity
module Bson = Bson

type connection = {
  id : string;
  issuer : string;
  sso_url : string;
  entity_id : string;
  acs_url : string;
  org_id : string option;
  domains : string list;
  external_id_attribute : string option;
  email_attribute : string option;
  trust_email : bool;
  allow_jit : bool;
}

type t = { challenge : Challenge.t }

type error =
  | Invalid_connection of string
  | Invalid_state
  | Invalid_key_material of string
  | Response_too_large of int
  | Signing_error of string
  | Assertion_mismatch of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

let string_of_error = function
  | Invalid_connection s -> "Invalid SAML connection: " ^ s
  | Invalid_state -> "Invalid SAML state"
  | Invalid_key_material s -> "Invalid SAML key material: " ^ s
  | Response_too_large n -> "SAMLResponse exceeds " ^ string_of_int n ^ " bytes"
  | Signing_error s -> "SAML AuthnRequest signing failed: " ^ s
  | Assertion_mismatch s -> "SAML assertion mismatch: " ^ s
  | Challenge_error e -> Challenge.string_of_error e
  | Identity_error e -> Identity.string_of_error e

let make ~challenge : t = { challenge }

let trim = String.trim
let lower_trim s = String.lowercase_ascii (trim s)
let clean_domains xs = xs |> List.map lower_trim |> List.filter (( <> ) "")

let nonblank_opt s =
  let s = trim s in
  if s = "" then None else Some s

let connection ?org_id ?(domains = []) ?external_id_attribute ?(email_attribute = "email") ?(trust_email = false)
    ?(allow_jit = true) ~id ~issuer ~sso_url ~entity_id ~acs_url () =
  let id = lower_trim id in
  let issuer = trim issuer in
  let sso_url = trim sso_url in
  let entity_id = trim entity_id in
  let acs_url = trim acs_url in
  if id = "" then Error (Invalid_connection "id cannot be blank")
  else if issuer = "" then Error (Invalid_connection "issuer cannot be blank")
  else if sso_url = "" then Error (Invalid_connection "sso_url cannot be blank")
  else if entity_id = "" then Error (Invalid_connection "entity_id cannot be blank")
  else if acs_url = "" then Error (Invalid_connection "acs_url cannot be blank")
  else
    Ok
      {
        id;
        issuer;
        sso_url;
        entity_id;
        acs_url;
        org_id = Option.bind org_id nonblank_opt;
        domains = clean_domains domains;
        external_id_attribute = Option.bind external_id_attribute nonblank_opt;
        email_attribute = nonblank_opt email_attribute;
        trust_email;
        allow_jit;
      }

let trusted_keys_of_pem pem =
  let public_key cert_error =
    match X509.Public_key.decode_pem pem with
    | Ok key -> Ok [ key ]
    | Error (`Msg key_error) -> Error (Invalid_key_material (cert_error ^ "; " ^ key_error))
  in
  match X509.Certificate.decode_pem_multiple pem with
  | Ok [] -> public_key "PEM did not contain certificates"
  | Ok certs -> Ok (List.map X509.Certificate.public_key certs)
  | Error (`Msg cert_error) -> public_key cert_error

let bson_string key value = (key, Bson.String value)

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts.Saml: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let metadata ~(connection : connection) ~request_id ?user_id ?org_id ?redirect () : Challenge.metadata =
  let org_id = match org_id with Some _ -> org_id | None -> connection.org_id in
  {
    user_id;
    email = None;
    org_id;
    connection_id = Some connection.id;
    redirect;
    data =
      [
        bson_string "request_id" request_id;
        bson_string "connection_id" connection.id;
        bson_string "issuer" connection.issuer;
        bson_string "entity_id" connection.entity_id;
        bson_string "acs_url" connection.acs_url;
      ];
  }

type request = {
  request_id : string;
  relay_state : Challenge.token;
  record : Challenge.record;
  connection : connection;
}

let request_id_of_record_id id = "_" ^ id

let issue_request (t : t) ?ttl ?user_id ?org_id ?redirect (connection : connection) =
  let request_id = request_id_of_record_id (b64url (secure_random 18)) in
  match Challenge.create t.challenge ~purpose:Challenge.Saml_request ~metadata:(metadata ~connection ~request_id ?user_id ?org_id ?redirect ()) ?ttl () with
  | Error e -> Error (Challenge_error e)
  | Ok issued -> Ok { request_id; relay_state = issued.token; record = issued.record; connection }

let xml_escape_text s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let xml_escape_attr s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\t' -> Buffer.add_string b "&#x9;"
      | '\n' -> Buffer.add_string b "&#xA;"
      | '\r' -> Buffer.add_string b "&#xD;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let authn_request_xml (request : request) =
  let c = request.connection in
  Printf.sprintf
    {|<samlp:AuthnRequest xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol" xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion" ID="%s" Version="2.0" ProtocolBinding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST" AssertionConsumerServiceURL="%s" Destination="%s"><saml:Issuer>%s</saml:Issuer><samlp:NameIDPolicy AllowCreate="true"/></samlp:AuthnRequest>|}
    (xml_escape_attr request.request_id) (xml_escape_attr c.acs_url) (xml_escape_attr c.sso_url)
    (xml_escape_text c.entity_id)

let zlib_run (t : 'a Zlib.t) (input : string) flush =
  let n = String.length input in
  let inbuf = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout (max 1 n) in
  if n > 0 then Bigstringaf.blit_from_string input ~src_off:0 inbuf ~dst_off:0 ~len:n;
  t.Zlib.in_buf <- inbuf;
  t.Zlib.in_ofs <- 0;
  t.Zlib.in_len <- n;
  let cap = 0x4000 in
  let out = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout cap in
  let buf = Buffer.create (max 64 n) in
  let rec loop () =
    t.Zlib.out_buf <- out;
    t.Zlib.out_ofs <- 0;
    t.Zlib.out_len <- cap;
    let st = Zlib.flate t flush in
    if t.Zlib.out_ofs > 0 then Buffer.add_string buf (Bigstringaf.substring out ~off:0 ~len:t.Zlib.out_ofs);
    match st with
    | Zlib.Stream_end -> Buffer.contents buf
    | Zlib.Ok -> loop ()
    | Zlib.Buf_error -> Buffer.contents buf
    | Zlib.Need_dict -> failwith "zlib: need dict"
    | Zlib.Data_error m -> failwith ("zlib: " ^ m)
  in
  loop ()

let raw_deflate s = zlib_run (Zlib.create_deflate ~window_bits:(-15) ()) s Zlib.Finish

let redirect_url (request : request) =
  let saml_request = Base64.encode_string (raw_deflate (authn_request_xml request)) in
  let relay_state = Challenge.token_to_string request.relay_state in
  let q =
    "SAMLRequest=" ^ Fennec_core.Http.percent_encode saml_request ^ "&RelayState="
    ^ Fennec_core.Http.percent_encode relay_state
  in
  request.connection.sso_url ^ (if String.contains request.connection.sso_url '?' then "&" else "?") ^ q

let signed_redirect_url (request : request) ~signing_key =
  let saml_request = Base64.encode_string (raw_deflate (authn_request_xml request)) in
  let relay_state = Challenge.token_to_string request.relay_state in
  let sig_alg = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" in
  let q =
    "SAMLRequest=" ^ Fennec_core.Http.percent_encode saml_request ^ "&RelayState="
    ^ Fennec_core.Http.percent_encode relay_state ^ "&SigAlg=" ^ Fennec_core.Http.percent_encode sig_alg
  in
  match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 signing_key (`Message q) with
  | Error (`Msg msg) -> Error (Signing_error msg)
  | Ok signature ->
    let q = q ^ "&Signature=" ^ Fennec_core.Http.percent_encode (Base64.encode_string signature) in
    Ok (request.connection.sso_url ^ (if String.contains request.connection.sso_url '?' then "&" else "?") ^ q)

type state = {
  request_id : string;
  connection_id : string;
  issuer : string;
  entity_id : string;
  acs_url : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

let data_string key data = match List.assoc_opt key data with Some (Bson.String v) -> Some v | _ -> None

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_state
  | Some 0 -> Error Invalid_state
  | Some i -> Ok (String.sub raw 0 i)

let state_of_record ?expected_connection record =
  let data = record.Challenge.metadata.data in
  match
    ( data_string "request_id" data,
      data_string "connection_id" data,
      data_string "issuer" data,
      data_string "entity_id" data,
      data_string "acs_url" data )
  with
  | Some request_id, Some connection_id, Some issuer, Some entity_id, Some acs_url ->
    let connection_id = lower_trim connection_id in
    let expected_ok =
      match expected_connection with
      | None -> true
      | Some expected -> connection_id = lower_trim expected
    in
    if not expected_ok then Error Invalid_state
    else
      Ok
        {
          request_id;
          connection_id;
          issuer;
          entity_id;
          acs_url;
          user_id = record.metadata.user_id;
          org_id = record.metadata.org_id;
          redirect = record.metadata.redirect;
          record;
        }
  | _ -> Error Invalid_state

let state_for_token (t : t) ?expected_connection token =
  match token_id token with
  | Error _ as e -> e
  | Ok id -> (
    match Challenge.find t.challenge id with
    | Error e -> Error (Challenge_error e)
    | Ok None -> Error Invalid_state
    | Ok (Some record) -> state_of_record ?expected_connection record)

let consume_state (t : t) ?expected_connection token =
  match state_for_token t ?expected_connection token with
  | Error _ as e -> e
  | Ok _ -> (
    match Challenge.consume t.challenge ~purpose:Challenge.Saml_request token with
    | Error e -> Error (Challenge_error e)
    | Ok record -> state_of_record ?expected_connection record)

type assertion = {
  issuer : string;
  audience : string;
  recipient : string;
  destination : string option;
  in_response_to : string option;
  not_before : float option;
  not_on_or_after : float option;
  name_id : string;
  name_id_format : string option;
  external_id : string option;
  email : string option;
  attributes : (string * string list) list;
  session_index : string option;
}

type principal = {
  identity : Identity.key;
  email_identity : Identity.key option;
  email : string option;
  allow_jit : bool;
  org_id : string option;
  session_index : string option;
  signature_key_fingerprint : string option;
  attributes : (string * string list) list;
  assertion : assertion;
}

let option_exists f = function Some x -> f x | None -> false

let email_domain email =
  match String.index_opt email '@' with
  | None -> None
  | Some i when i = String.length email - 1 -> None
  | Some i -> Some (String.sub email (i + 1) (String.length email - i - 1) |> lower_trim)

let domain_allowed (connection : connection) (assertion : assertion) =
  match connection.domains with
  | [] -> true
  | domains -> (
    match Option.bind assertion.email email_domain with
    | None -> false
    | Some domain -> List.exists (( = ) domain) domains)

let validate_assertion ?(now = Unix.gettimeofday) ?(leeway = 60.) (connection : connection) (state : state)
    (assertion : assertion) =
  let current = now () in
  if assertion.issuer <> connection.issuer || state.issuer <> connection.issuer then Error (Assertion_mismatch "issuer")
  else if state.connection_id <> connection.id then Error (Assertion_mismatch "connection")
  else if state.entity_id <> connection.entity_id then Error (Assertion_mismatch "entity_id")
  else if state.acs_url <> connection.acs_url then Error (Assertion_mismatch "acs_url")
  else if assertion.audience <> connection.entity_id then Error (Assertion_mismatch "audience")
  else if assertion.recipient <> connection.acs_url then Error (Assertion_mismatch "recipient")
  else if option_exists (( <> ) connection.acs_url) assertion.destination then Error (Assertion_mismatch "destination")
  else if assertion.in_response_to <> Some state.request_id then Error (Assertion_mismatch "in_response_to")
  else if option_exists (fun not_before -> not_before -. leeway > current) assertion.not_before then Error (Assertion_mismatch "not_before")
  else if option_exists (fun not_on_or_after -> not_on_or_after +. leeway <= current) assertion.not_on_or_after then Error (Assertion_mismatch "not_on_or_after")
  else if not (domain_allowed connection assertion) then Error (Assertion_mismatch "domain")
  else
    match Identity.saml ~connection:connection.id ~name_id:assertion.name_id ?external_id:assertion.external_id () with
    | Error e -> Error (Identity_error e)
    | Ok identity -> (
      match assertion.email with
      | Some email -> (
        match Identity.email ~verified:connection.trust_email email with
        | Error e -> Error (Identity_error e)
        | Ok email_identity ->
          Ok
            {
              identity;
              email_identity = Some email_identity;
              email = Some (Identity.subject email_identity);
              allow_jit = connection.allow_jit;
              org_id = state.org_id;
              session_index = assertion.session_index;
              signature_key_fingerprint = None;
              attributes = assertion.attributes;
              assertion;
            })
      | None ->
        Ok
          {
            identity;
            email_identity = None;
            email = None;
            allow_jit = connection.allow_jit;
            org_id = state.org_id;
            session_index = assertion.session_index;
            signature_key_fingerprint = None;
            attributes = assertion.attributes;
            assertion;
          })

type xml = {
  name : string;
  attrs : (string * string) list;
  ns : (string * string) list;
  children : node list;
}

and node = Elem of xml | Text of string

let local_name name =
  match String.rindex_opt name ':' with
  | None -> name
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)

let prefix_name name = match String.rindex_opt name ':' with None -> "" | Some i -> String.sub name 0 i

let decode_entity = function
  | "amp" -> Some "&"
  | "lt" -> Some "<"
  | "gt" -> Some ">"
  | "quot" -> Some "\""
  | "apos" -> Some "'"
  | e when String.length e > 1 && e.[0] = '#' -> (
    let n =
      if String.length e > 2 && (e.[1] = 'x' || e.[1] = 'X') then int_of_string_opt ("0" ^ String.sub e 1 (String.length e - 1))
      else int_of_string_opt (String.sub e 1 (String.length e - 1))
    in
    match n with
    | Some n when n >= 0 && n <= 0x7f -> Some (String.make 1 (Char.chr n))
    | _ -> None)
  | _ -> None

let xml_decode s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then Some (Buffer.contents b)
    else if s.[i] <> '&' then (
      Buffer.add_char b s.[i];
      loop (i + 1))
    else
      match String.index_from_opt s i ';' with
      | None -> None
      | Some j -> (
        match decode_entity (String.sub s (i + 1) (j - i - 1)) with
        | None -> None
        | Some decoded ->
          Buffer.add_string b decoded;
          loop (j + 1))
  in
  loop 0

let parse_attrs s =
  let n = String.length s in
  let rec skip_ws i = if i < n && (s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' || s.[i] = '\r') then skip_ws (i + 1) else i in
  let rec name_end i =
    if i < n && not (s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' || s.[i] = '\r' || s.[i] = '=') then name_end (i + 1) else i
  in
  let rec loop i acc =
    let i = skip_ws i in
    if i >= n then Some (List.rev acc)
    else
      let j = name_end i in
      if j = i then None
      else
        let k = skip_ws j in
        if k >= n || s.[k] <> '=' then None
        else
          let v0 = skip_ws (k + 1) in
          if v0 >= n || (s.[v0] <> '"' && s.[v0] <> '\'') then None
          else
            let quote = s.[v0] in
            match String.index_from_opt s (v0 + 1) quote with
            | None -> None
            | Some v1 -> (
              match xml_decode (String.sub s (v0 + 1) (v1 - v0 - 1)) with
              | None -> None
              | Some value -> loop (v1 + 1) ((String.sub s i (j - i), value) :: acc))
  in
  loop 0 []

let parse_start_tag inside =
  let inside = String.trim inside in
  let self_close = String.length inside > 0 && inside.[String.length inside - 1] = '/' in
  let body = if self_close then String.sub inside 0 (String.length inside - 1) |> String.trim else inside in
  let n = String.length body in
  let rec name_end i =
    if i < n && not (body.[i] = ' ' || body.[i] = '\t' || body.[i] = '\n' || body.[i] = '\r') then name_end (i + 1) else i
  in
  let j = name_end 0 in
  if j = 0 then None
  else
    let name = String.sub body 0 j in
    match parse_attrs (String.sub body j (n - j)) with
    | None -> None
    | Some attrs -> Some (name, attrs, self_close)

let ns_prefix = function "xmlns" -> Some "" | s when String.length s > 6 && String.sub s 0 6 = "xmlns:" -> Some (String.sub s 6 (String.length s - 6)) | _ -> None

let apply_ns parent attrs =
  let decls, attrs =
    List.fold_left
      (fun (decls, attrs) (k, v) ->
        match ns_prefix k with
        | Some p -> ((p, v) :: decls, attrs)
        | None -> (decls, (k, v) :: attrs))
      ([], []) attrs
  in
  let ns =
    List.fold_left
      (fun env (p, v) -> (p, v) :: List.remove_assoc p env)
      parent (List.rev decls)
  in
  (List.rev attrs, ns)

let parse_xml input =
  if String.contains input '\000' || String.contains input '\x00' then Error (Assertion_mismatch "xml")
  else
    let n = String.length input in
    let add_text txt stack =
      match xml_decode txt with
      | None -> None
      | Some "" -> Some stack
      | Some text -> (
        match stack with
        | [] -> if String.trim text = "" then Some [] else None
        | (name, attrs, ns, children) :: rest -> Some ((name, attrs, ns, Text text :: children) :: rest))
    in
    let add_elem elem stack =
      match stack with
      | [] -> [ ("$root", [], [], [ Elem elem ]) ]
      | (name, attrs, ns, children) :: rest -> (name, attrs, ns, Elem elem :: children) :: rest
    in
    let rec loop i stack =
      if i >= n then
        match stack with
        | [ ("$root", _, _, [ Elem root ]) ] -> Ok root
        | _ -> Error (Assertion_mismatch "xml")
      else
        match String.index_from_opt input i '<' with
        | None -> (
          match add_text (String.sub input i (n - i)) stack with
          | None -> Error (Assertion_mismatch "xml")
          | Some stack -> loop n stack)
        | Some lt -> (
          match add_text (String.sub input i (lt - i)) stack with
          | None -> Error (Assertion_mismatch "xml")
          | Some stack ->
            if lt + 1 < n && input.[lt + 1] = '?' then (
              match String.index_from_opt input (lt + 2) '>' with None -> Error (Assertion_mismatch "xml") | Some gt -> loop (gt + 1) stack)
            else if lt + 3 < n && String.sub input (lt + 1) 3 = "!--" then (
              let rec find_comment j =
                if j + 2 >= n then None
                else if input.[j] = '-' && input.[j + 1] = '-' && input.[j + 2] = '>' then Some j
                else find_comment (j + 1)
              in
              match find_comment (lt + 4) with None -> Error (Assertion_mismatch "xml") | Some j -> loop (j + 3) stack)
            else if lt + 2 < n && input.[lt + 1] = '!' then Error (Assertion_mismatch "xml")
            else
              match String.index_from_opt input (lt + 1) '>' with
              | None -> Error (Assertion_mismatch "xml")
              | Some gt ->
                let inside = String.sub input (lt + 1) (gt - lt - 1) in
                if String.length inside > 0 && inside.[0] = '/' then
                  let close_name = String.trim (String.sub inside 1 (String.length inside - 1)) in
                  match stack with
                  | (name, attrs, ns, children) :: rest when name = close_name ->
                    let elem = { name; attrs; ns; children = List.rev children } in
                    loop (gt + 1) (add_elem elem rest)
                  | _ -> Error (Assertion_mismatch "xml")
                else
                  match parse_start_tag inside with
                  | None -> Error (Assertion_mismatch "xml")
                  | Some (name, raw_attrs, self_close) ->
                    let parent_ns = match stack with ( _, _, ns, _ ) :: _ -> ns | [] -> [] in
                    let attrs, ns = apply_ns parent_ns raw_attrs in
                    if self_close then loop (gt + 1) (add_elem { name; attrs; ns; children = [] } stack)
                    else loop (gt + 1) ((name, attrs, ns, []) :: stack))
    in
    loop 0 []

let text_of_nodes nodes =
  let b = Buffer.create 32 in
  let rec add = function
    | Text s -> Buffer.add_string b s
    | Elem e -> List.iter add e.children
  in
  List.iter add nodes;
  Buffer.contents b |> String.trim

let child_named name e =
  List.find_map (function Elem c when local_name c.name = name -> Some c | _ -> None) e.children

let children_named name e =
  List.filter_map (function Elem c when local_name c.name = name -> Some c | _ -> None) e.children

let attr name e = List.assoc_opt name e.attrs
let text_child name e = Option.map (fun c -> text_of_nodes c.children) (child_named name e)

let visibly_used_prefixes e =
  let add_name name acc =
    match prefix_name name with
    | "" -> acc
    | p -> if List.mem p acc then acc else p :: acc
  in
  List.fold_left (fun acc (k, _) -> add_name k acc) (add_name e.name []) e.attrs

let canon_escape_text = xml_escape_text
let canon_escape_attr = xml_escape_attr

let canonicalize ?(omit_signature = false) root =
  let b = Buffer.create 256 in
  let rec elem e =
    if omit_signature && local_name e.name = "Signature" then ()
    else (
      Buffer.add_char b '<';
      Buffer.add_string b e.name;
      let prefixes = visibly_used_prefixes e in
      let ns_attrs =
        List.filter_map
          (fun p ->
            match List.assoc_opt p e.ns with
            | None -> None
            | Some uri -> Some ((if p = "" then "xmlns" else "xmlns:" ^ p), uri))
          prefixes
      in
      let attrs = List.sort compare (ns_attrs @ e.attrs) in
      List.iter
        (fun (k, v) ->
          Buffer.add_char b ' ';
          Buffer.add_string b k;
          Buffer.add_string b "=\"";
          Buffer.add_string b (canon_escape_attr v);
          Buffer.add_char b '"')
        attrs;
      Buffer.add_char b '>';
      List.iter node e.children;
      Buffer.add_string b "</";
      Buffer.add_string b e.name;
      Buffer.add_char b '>')
  and node = function Text s -> Buffer.add_string b (canon_escape_text s) | Elem e -> elem e in
  elem root;
  Buffer.contents b

let b64d s = match Base64.decode s with Ok x -> Some x | Error _ -> None
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

let rec elements acc e =
  let acc = e :: acc in
  List.fold_left (fun acc -> function Elem c -> elements acc c | Text _ -> acc) acc e.children

let signatures root = List.filter (fun e -> local_name e.name = "Signature") (elements [] root)

let direct_signature e =
  match children_named "Signature" e with
  | [ s ] -> Some s
  | [] -> None
  | _ -> None

let ids root =
  let all = elements [] root in
  List.filter_map
    (fun e ->
      match attr "ID" e with
      | Some id -> Some (id, e)
      | None -> None)
    all

let unique_id ids id =
  match List.filter (fun (id', _) -> id' = id) ids with
  | [ (_, e) ] -> Some e
  | _ -> None

let child_text_required name e =
  match text_child name e with
  | Some s when s <> "" -> Ok s
  | _ -> Error (Assertion_mismatch name)

let algorithm e = attr "Algorithm" e

let verify_xml_signature ~trusted_keys root =
  match signatures root with
  | [ signature ] -> (
    let signed_info = child_named "SignedInfo" signature in
    let signature_value = child_text_required "SignatureValue" signature in
    match (signed_info, signature_value) with
    | Some signed_info, Ok signature_value -> (
      match (child_named "CanonicalizationMethod" signed_info, child_named "SignatureMethod" signed_info, children_named "Reference" signed_info) with
      | Some canon_method, Some sig_method, [ reference ]
        when algorithm canon_method = Some "http://www.w3.org/2001/10/xml-exc-c14n#"
             && algorithm sig_method = Some "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" -> (
        match attr "URI" reference with
        | Some uri when String.length uri > 1 && uri.[0] = '#' -> (
          let ref_id = String.sub uri 1 (String.length uri - 1) in
          let id_index = ids root in
          match unique_id id_index ref_id with
          | None -> Error (Assertion_mismatch "reference")
          | Some target -> (
            match direct_signature target with
            | None -> Error (Assertion_mismatch "signature_scope")
            | Some s when s != signature -> Error (Assertion_mismatch "signature_scope")
            | Some _ -> (
              let transforms = Option.value ~default:[] (Option.map (children_named "Transform") (child_named "Transforms" reference)) in
              let transform_algs = List.filter_map algorithm transforms in
              if
                transform_algs
                <> [ "http://www.w3.org/2000/09/xmldsig#enveloped-signature"; "http://www.w3.org/2001/10/xml-exc-c14n#" ]
              then Error (Assertion_mismatch "transforms")
              else
                match (child_named "DigestMethod" reference, child_text_required "DigestValue" reference, b64d signature_value) with
                | Some digest_method, Ok digest_value, Some signature_bytes
                  when algorithm digest_method = Some "http://www.w3.org/2001/04/xmlenc#sha256" ->
                  let target_canon = canonicalize ~omit_signature:true target in
                  let actual_digest = Base64.encode_string (sha256 target_canon) in
                  if not (constant_eq digest_value actual_digest) then Error (Assertion_mismatch "digest")
                  else
                    let signed_info_canon = canonicalize signed_info in
                    let verified_key =
                      List.find_opt
                        (fun key ->
                          match X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1 ~signature:signature_bytes key (`Message signed_info_canon) with
                          | Ok () -> true
                          | Error _ -> false)
                        trusted_keys
                    in
                    (match verified_key with
                    | Some key -> Ok (target, hex (X509.Public_key.fingerprint key))
                    | None -> Error (Assertion_mismatch "signature"))
                | _ -> Error (Assertion_mismatch "digest"))))
        | _ -> Error (Assertion_mismatch "reference"))
      | _ -> Error (Assertion_mismatch "signature_method"))
    | _ -> Error (Assertion_mismatch "signature"))
  | [] -> Error (Assertion_mismatch "signature")
  | _ -> Error (Assertion_mismatch "signature_count")

let first_attribute_value names attrs =
  let names = List.map lower_trim names in
  List.find_map
    (fun (name, values) ->
      if List.mem (lower_trim name) names then List.find_opt (fun s -> String.trim s <> "") values else None)
    attrs

let saml_time s =
  match Ptime.of_rfc3339 ~strict:false s with
  | Ok (t, _, _) -> Some (Ptime.to_float_s t)
  | Error _ -> None

let extract_assertion (connection : connection) root signed =
  let response = if local_name root.name = "Response" then root else root in
  if local_name response.name <> "Response" then Error (Assertion_mismatch "response")
  else if child_named "EncryptedAssertion" response <> None then Error (Assertion_mismatch "encrypted_assertion")
  else if
    Option.bind (child_named "Status" response) (fun status -> Option.bind (child_named "StatusCode" status) (attr "Value"))
    <> Some "urn:oasis:names:tc:SAML:2.0:status:Success"
  then Error (Assertion_mismatch "status")
  else
    let assertions = children_named "Assertion" response in
    match assertions with
    | [ assertion_elem ] ->
      let signed_ok = signed == response || signed == assertion_elem in
      if not signed_ok then Error (Assertion_mismatch "signed_element")
      else
        let issuer = text_child "Issuer" assertion_elem |> Option.value ~default:(text_child "Issuer" response |> Option.value ~default:"") in
        let subject = child_named "Subject" assertion_elem in
        let conditions = child_named "Conditions" assertion_elem in
        let name_id =
          Option.bind subject (fun s -> child_named "NameID" s)
        in
        let bearer_data =
          Option.bind subject (fun s ->
              children_named "SubjectConfirmation" s
              |> List.find_map (fun sc ->
                     if attr "Method" sc = Some "urn:oasis:names:tc:SAML:2.0:cm:bearer" then child_named "SubjectConfirmationData" sc
                     else None))
        in
        let audience =
          Option.bind conditions (fun c -> Option.bind (child_named "AudienceRestriction" c) (fun ar -> text_child "Audience" ar))
          |> Option.value ~default:""
        in
        let attributes =
          children_named "AttributeStatement" assertion_elem
          |> List.concat_map (fun st ->
                 children_named "Attribute" st
                 |> List.filter_map (fun a ->
                        let name = Option.value (attr "Name" a) ~default:(Option.value (attr "FriendlyName" a) ~default:"") in
                        if name = "" then None
                        else Some (name, List.map (fun v -> text_of_nodes v.children) (children_named "AttributeValue" a))))
        in
        let configured_email = Option.bind connection.email_attribute (fun n -> first_attribute_value [ n ] attributes) in
        let email =
          match configured_email with
          | Some _ as e -> e
          | None -> (
            match name_id with
            | Some n when attr "Format" n = Some "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress" -> Some (text_of_nodes n.children)
            | _ -> first_attribute_value [ "mail"; "emailaddress" ] attributes)
        in
        let external_id = Option.bind connection.external_id_attribute (fun n -> first_attribute_value [ n ] attributes) in
        let assertion =
          {
            issuer;
            audience;
            recipient = Option.value ~default:"" (Option.bind bearer_data (attr "Recipient"));
            destination = attr "Destination" response;
            in_response_to =
              (match Option.bind bearer_data (attr "InResponseTo") with Some _ as x -> x | None -> attr "InResponseTo" response);
            not_before = Option.bind conditions (fun c -> Option.bind (attr "NotBefore" c) saml_time);
            not_on_or_after =
              (match Option.bind bearer_data (fun b -> Option.bind (attr "NotOnOrAfter" b) saml_time) with
              | Some _ as x -> x
              | None -> Option.bind conditions (fun c -> Option.bind (attr "NotOnOrAfter" c) saml_time));
            name_id = Option.map (fun n -> text_of_nodes n.children) name_id |> Option.value ~default:"";
            name_id_format = Option.bind name_id (attr "Format");
            external_id;
            email;
            attributes;
            session_index = Option.bind (child_named "AuthnStatement" assertion_elem) (attr "SessionIndex");
          }
        in
        Ok assertion
    | [] -> Error (Assertion_mismatch "assertion")
    | _ -> Error (Assertion_mismatch "assertion_count")

let default_max_response_bytes = 256 * 1024

let verify_response ?now ?leeway ?(max_response_bytes = default_max_response_bytes) connection state ~trusted_keys ~saml_response =
  let encoded_limit = ((max_response_bytes + 2) / 3 * 4) + 16 in
  if String.length saml_response > encoded_limit then Error (Response_too_large max_response_bytes)
  else
    match b64d saml_response with
  | None -> Error (Assertion_mismatch "base64")
    | Some xml -> (
      if String.length xml > max_response_bytes then Error (Response_too_large max_response_bytes)
      else
        match parse_xml xml with
        | Error _ as e -> e
        | Ok root -> (
          match verify_xml_signature ~trusted_keys root with
          | Error _ as e -> e
          | Ok (signed, signature_key_fingerprint) -> (
            match extract_assertion connection root signed with
            | Error _ as e -> e
            | Ok assertion -> (
              match validate_assertion ?now ?leeway connection state assertion with
              | Error _ as e -> e
              | Ok principal -> Ok { principal with signature_key_fingerprint = Some signature_key_fingerprint }))))

let consume_response (t : t) ?now ?leeway ?max_response_bytes ?expected_connection connection ~trusted_keys ~relay_state ~saml_response =
  match state_for_token t ?expected_connection relay_state with
  | Error _ as e -> e
  | Ok state -> (
    match verify_response ?now ?leeway ?max_response_bytes connection state ~trusted_keys ~saml_response with
    | Error _ as e -> e
    | Ok principal -> (
      match Challenge.consume t.challenge ~purpose:Challenge.Saml_request relay_state with
      | Error e -> Error (Challenge_error e)
      | Ok record -> (
        match state_of_record ?expected_connection record with
        | Error _ as e -> e
        | Ok consumed when consumed.request_id = state.request_id -> Ok principal
        | Ok _ -> Error Invalid_state)))

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"saml-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_connection () =
  ok
    (connection ~id:" Corp " ~issuer:"https://idp.example/saml" ~sso_url:"https://idp.example/sso"
       ~entity_id:"https://app.test/saml/metadata" ~acs_url:"https://app.test/saml/acs" ~org_id:"org_1"
       ~domains:[ "Example.COM" ] ~trust_email:true ())

let assertion ?(issuer = "https://idp.example/saml") ?(audience = "https://app.test/saml/metadata")
    ?(recipient = "https://app.test/saml/acs") ?destination ?in_response_to ?not_before ?not_on_or_after
    ?(name_id = "ada@example.com") ?name_id_format ?external_id ?email ?(attributes = []) ?session_index () =
  {
    issuer;
    audience;
    recipient;
    destination;
    in_response_to;
    not_before;
    not_on_or_after;
    name_id;
    name_id_format;
    external_id;
    email;
    attributes;
    session_index;
  }

let%test "connection normalizes id and domains" =
  let c = test_connection () in
  c.id = "corp" && c.domains = [ "example.com" ] && c.trust_email

let%test "connection rejects blank required fields" =
  Result.is_error (connection ~id:"" ~issuer:"i" ~sso_url:"s" ~entity_id:"e" ~acs_url:"a" ())
  && Result.is_error (connection ~id:"x" ~issuer:"" ~sso_url:"s" ~entity_id:"e" ~acs_url:"a" ())

let%test "trusted_keys_of_pem accepts public key PEM" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  match trusted_keys_of_pem (X509.Public_key.encode_pem (X509.Private_key.public key)) with
  | Ok [ parsed ] -> hex (X509.Public_key.fingerprint parsed) = hex (X509.Public_key.fingerprint (X509.Private_key.public key))
  | _ -> false

let%test "trusted_keys_of_pem rejects invalid PEM" =
  trusted_keys_of_pem "not pem" |> Result.is_error

let%test "issue_request stores public request id and relay state" =
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t ~user_id:"user_1" c with
  | Error _ -> false
  | Ok r ->
    String.length r.request_id > 1
    && r.request_id.[0] = '_'
    && r.record.metadata.connection_id = Some "corp"
    && data_string "request_id" r.record.metadata.data = Some r.request_id

let%test "consume_state is purpose-bound and single-use" =
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t c with
  | Error _ -> false
  | Ok r -> (
    match consume_state t ~expected_connection:"corp" r.relay_state with
    | Error _ -> false
    | Ok state ->
      state.connection_id = "corp"
      && state.request_id = r.request_id
      && consume_state t r.relay_state = Error (Challenge_error Challenge.Already_consumed))

let%test "wrong connection does not consume state" =
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t c with
  | Error _ -> false
  | Ok r -> consume_state t ~expected_connection:"other" r.relay_state = Error Invalid_state && Result.is_ok (consume_state t ~expected_connection:"corp" r.relay_state)

let%test "expired request state fails closed" =
  let t, set_now = test_service ~ttl:10. () in
  let c = test_connection () in
  match issue_request t c with
  | Error _ -> false
  | Ok r ->
    set_now 1_011.;
    consume_state t r.relay_state = Error (Challenge_error Challenge.Expired)

let valid_state () =
  let t, c, r =
    let t, _ = test_service () in
    let c = test_connection () in
    match issue_request t c with
    | Error e -> failwith (string_of_error e)
    | Ok r -> (t, c, r)
  in
  (c, ok (consume_state t r.relay_state))

let valid_request () =
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t c with
  | Error e -> failwith (string_of_error e)
  | Ok r -> (t, c, r)

let%test "validate_assertion derives saml and verified email identities" =
  let c, state = valid_state () in
  match
    validate_assertion ~now:(fun () -> 1_000.) c state
      (assertion ~in_response_to:state.request_id ~not_on_or_after:1_200. ~external_id:"stable-id"
         ~email:"Ada@Example.COM" ~session_index:"sess" ())
  with
  | Error _ -> false
  | Ok p ->
    Identity.kind p.identity = Identity.Saml
    && Identity.namespace p.identity = Some "corp"
    && Identity.subject p.identity = "stable-id"
    && p.email = Some "ada@example.com"
    && option_exists Identity.is_verified_email p.email_identity
    && p.session_index = Some "sess"

let%test "validate_assertion rejects core mismatches" =
  let c, state = valid_state () in
  validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~issuer:"other" ~in_response_to:state.request_id ())
  = Error (Assertion_mismatch "issuer")
  && validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~audience:"other" ~in_response_to:state.request_id ())
     = Error (Assertion_mismatch "audience")
  && validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~recipient:"other" ~in_response_to:state.request_id ())
     = Error (Assertion_mismatch "recipient")
  && validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~in_response_to:"other" ()) = Error (Assertion_mismatch "in_response_to")

let%test "validate_assertion enforces time and domain policy" =
  let c, state = valid_state () in
  validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~in_response_to:state.request_id ~not_before:1_200. ())
  = Error (Assertion_mismatch "not_before")
  && validate_assertion ~now:(fun () -> 1_000.) c state (assertion ~in_response_to:state.request_id ~not_on_or_after:900. ())
     = Error (Assertion_mismatch "not_on_or_after")
  && validate_assertion ~now:(fun () -> 1_000.) c state
       (assertion ~in_response_to:state.request_id ~not_on_or_after:1_200. ~email:"ada@other.test" ())
     = Error (Assertion_mismatch "domain")

let must_parse_xml s = match parse_xml s with Ok x -> x | Error e -> failwith (string_of_error e)

let signed_info_xml ~uri ~digest =
  Printf.sprintf
    {|<ds:SignedInfo xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"></ds:CanonicalizationMethod><ds:SignatureMethod Algorithm="http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"></ds:SignatureMethod><ds:Reference URI="#%s"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"></ds:Transform><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"></ds:Transform></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2001/04/xmlenc#sha256"></ds:DigestMethod><ds:DigestValue>%s</ds:DigestValue></ds:Reference></ds:SignedInfo>|}
    (xml_escape_attr uri) digest

let signature_xml ~key ~target_xml ~target_id =
  let target = must_parse_xml target_xml in
  let digest = Base64.encode_string (sha256 (canonicalize target)) in
  let signed_info = signed_info_xml ~uri:target_id ~digest in
  let signed_info_canon = canonicalize (must_parse_xml signed_info) in
  let signature =
    match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 key (`Message signed_info_canon) with
    | Ok s -> Base64.encode_string s
    | Error (`Msg m) -> failwith m
  in
  Printf.sprintf
    {|<ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#">%s<ds:SignatureValue>%s</ds:SignatureValue></ds:Signature>|}
    signed_info signature

let assertion_xml ?(signature = "") ~id ~request_id () =
  Printf.sprintf
    {|<saml:Assertion xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion" ID="%s" Version="2.0"><saml:Issuer>https://idp.example/saml</saml:Issuer>%s<saml:Subject><saml:NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress">ada@example.com</saml:NameID><saml:SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer"><saml:SubjectConfirmationData InResponseTo="%s" Recipient="https://app.test/saml/acs" NotOnOrAfter="1970-01-01T00:20:00Z"></saml:SubjectConfirmationData></saml:SubjectConfirmation></saml:Subject><saml:Conditions NotBefore="1970-01-01T00:10:00Z" NotOnOrAfter="1970-01-01T00:20:00Z"><saml:AudienceRestriction><saml:Audience>https://app.test/saml/metadata</saml:Audience></saml:AudienceRestriction></saml:Conditions><saml:AuthnStatement SessionIndex="sess"></saml:AuthnStatement><saml:AttributeStatement><saml:Attribute Name="email"><saml:AttributeValue>ada@example.com</saml:AttributeValue></saml:Attribute><saml:Attribute Name="employee_id"><saml:AttributeValue>emp-1</saml:AttributeValue></saml:Attribute></saml:AttributeStatement></saml:Assertion>|}
    id signature (xml_escape_attr request_id)

let response_xml ?(id = "resp1") ?(status = "urn:oasis:names:tc:SAML:2.0:status:Success") ~assertion ~request_id () =
  Printf.sprintf
    {|<samlp:Response xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol" ID="%s" Version="2.0" Destination="https://app.test/saml/acs" InResponseTo="%s"><samlp:Issuer>https://idp.example/saml</samlp:Issuer><samlp:Status><samlp:StatusCode Value="%s"></samlp:StatusCode></samlp:Status>%s</samlp:Response>|}
    (xml_escape_attr id) (xml_escape_attr request_id) (xml_escape_attr status) assertion

let signed_response_xml ?(tamper = false) ?response_id ?response_status ~key ~state () =
  let unsigned_assertion = assertion_xml ~id:"assert1" ~request_id:state.request_id () in
  let signature = signature_xml ~key ~target_xml:unsigned_assertion ~target_id:"assert1" in
  let assertion = assertion_xml ~signature ~id:"assert1" ~request_id:state.request_id () in
  let xml = response_xml ?id:response_id ?status:response_status ~assertion ~request_id:state.request_id () in
  if tamper then String.map (function 'e' -> 'E' | c -> c) xml else xml

let signed_response ?tamper ?response_id ?response_status ~key ~state () =
  Base64.encode_string (signed_response_xml ?tamper ?response_id ?response_status ~key ~state ())

let replace_first ~needle ~replacement s =
  let needle_len = String.length needle in
  let rec find i =
    if i + needle_len > String.length s then None
    else if String.sub s i needle_len = needle then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> s
  | Some i ->
    String.sub s 0 i ^ replacement ^ String.sub s (i + needle_len) (String.length s - i - needle_len)

let%test "authn_request_xml and redirect_url carry request and relay state" =
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t c with
  | Error _ -> false
  | Ok r ->
    let xml = authn_request_xml r in
    String.contains xml '<'
    && String.contains (redirect_url r) '?'
    && String.contains (redirect_url r) '='
    && String.length (Challenge.token_to_string r.relay_state) > 10

let%test "signed_redirect_url signs redirect binding query" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let t, _ = test_service () in
  let c = test_connection () in
  match issue_request t c with
  | Error _ -> false
  | Ok r -> (
    match signed_redirect_url r ~signing_key:key with
    | Error _ -> false
    | Ok url -> (
      match String.index_opt url '?' with
      | None -> false
      | Some i ->
        let q = String.sub url (i + 1) (String.length url - i - 1) in
        let params = Fennec_core.Http.parse_query q in
        let q_to_sign =
          "SAMLRequest=" ^ Fennec_core.Http.percent_encode (List.assoc "SAMLRequest" params)
          ^ "&RelayState="
          ^ Fennec_core.Http.percent_encode (List.assoc "RelayState" params)
          ^ "&SigAlg="
          ^ Fennec_core.Http.percent_encode (List.assoc "SigAlg" params)
        in
        match (List.assoc_opt "Signature" params, Base64.decode (List.assoc "Signature" params)) with
        | Some _, Ok signature ->
          X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1 ~signature (X509.Private_key.public key) (`Message q_to_sign)
          = Ok ()
        | _ -> false))

let%test "verify_response accepts signed assertion end to end" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  let c = { c with external_id_attribute = Some "employee_id" } in
  match verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ] ~saml_response:(signed_response ~key ~state ()) with
  | Error _ -> false
  | Ok p ->
    Identity.kind p.identity = Identity.Saml
    && Identity.subject p.identity = "emp-1"
    && p.email = Some "ada@example.com"
    && option_exists Identity.is_verified_email p.email_identity
    && p.allow_jit
    && p.signature_key_fingerprint = Some (hex (X509.Public_key.fingerprint (X509.Private_key.public key)))

let%test "verify_response rejects tampered signed assertion" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  Result.is_error
    (verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ]
       ~saml_response:(signed_response ~key ~state ~tamper:true ()))

let%test "verify_response rejects wrong signing key" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let wrong = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public wrong ] ~saml_response:(signed_response ~key ~state ())
  = Error (Assertion_mismatch "signature")

let%test "verify_response rejects unsigned responses" =
  let c, state = valid_state () in
  let assertion = assertion_xml ~id:"assert1" ~request_id:state.request_id () in
  let saml_response = Base64.encode_string (response_xml ~assertion ~request_id:state.request_id ()) in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[] ~saml_response = Error (Assertion_mismatch "signature")

let%test "verify_response rejects oversized responses before XML parsing" =
  let c, state = valid_state () in
  verify_response ~max_response_bytes:8 ~now:(fun () -> 1_000.) c state ~trusted_keys:[] ~saml_response:(Base64.encode_string "<samlp:Response></samlp:Response>")
  = Error (Response_too_large 8)

let%test "verify_response rejects duplicate ids" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ]
    ~saml_response:(signed_response ~response_id:"assert1" ~key ~state ())
  = Error (Assertion_mismatch "reference")

let%test "verify_response rejects multiple signatures" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  let xml =
    signed_response_xml ~key ~state ()
    |> replace_first ~needle:"</samlp:Status>"
         ~replacement:
           {|</samlp:Status><ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"></ds:Signature>|}
  in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ] ~saml_response:(Base64.encode_string xml)
  = Error (Assertion_mismatch "signature_count")

let%test "verify_response rejects non-success status" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ]
    ~saml_response:(signed_response ~response_status:"urn:oasis:names:tc:SAML:2.0:status:Responder" ~key ~state ())
  = Error (Assertion_mismatch "status")

let%test "verify_response rejects encrypted assertions even with another valid signature" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let c, state = valid_state () in
  let xml =
    signed_response_xml ~key ~state ()
    |> replace_first ~needle:"<saml:Assertion"
         ~replacement:
           {|<saml:EncryptedAssertion xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"></saml:EncryptedAssertion><saml:Assertion|}
  in
  verify_response ~now:(fun () -> 1_000.) c state ~trusted_keys:[ X509.Private_key.public key ] ~saml_response:(Base64.encode_string xml)
  = Error (Assertion_mismatch "encrypted_assertion")

let%test "consume_response leaves relay state usable after invalid response and consumes valid response" =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let wrong = X509.Private_key.generate ~bits:2048 `RSA in
  let t, c, request = valid_request () in
  let state = ok (state_of_record request.record) in
  let bad =
    consume_response t ~now:(fun () -> 1_000.) c ~trusted_keys:[ X509.Private_key.public wrong ]
      ~relay_state:request.relay_state ~saml_response:(signed_response ~key ~state ())
  in
  let good =
    consume_response t ~now:(fun () -> 1_000.) c ~trusted_keys:[ X509.Private_key.public key ]
      ~relay_state:request.relay_state ~saml_response:(signed_response ~key ~state ())
  in
  let replay =
    consume_response t ~now:(fun () -> 1_000.) c ~trusted_keys:[ X509.Private_key.public key ]
      ~relay_state:request.relay_state ~saml_response:(signed_response ~key ~state ())
  in
  bad = Error (Assertion_mismatch "signature") && Result.is_ok good && replay = Error (Challenge_error Challenge.Already_consumed)
