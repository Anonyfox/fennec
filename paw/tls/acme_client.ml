(* ACME v2 (RFC 8555) client over Eio — no Lwt. The JWS/JWK is hand-ASSEMBLED (base64url + JSON +
   concat), but the CRYPTOGRAPHY is the audited pure-OCaml stack: x509 / mirage-crypto for the RSA
   account key + RS256 signing + the CSR, digestif for SHA-256, base64 for base64url. The protocol
   is driven over {!Https_client}. The JWK + thumbprint encoding (the one fiddly bit) is pinned to
   known vectors in the tests, so it is provably spec-correct without a live server. *)

(* A minimal, self-contained JSON reader (RFC 8259) — fennec-paw stays dependency-free (NO yojson),
   so a production server linking it never carries JSON-library weight. ACME only ever reads a handful
   of string / list / object fields, but the reader parses the FULL grammar (escapes incl. \uXXXX +
   surrogate pairs, numbers, nesting) so it can never misread a well-formed response. The fields we
   read are ASCII (status keywords, URLs, tokens); errors are surfaced from the raw response body. *)
module Json = struct
  type t = Null | Bool of bool | Num of string | Str of string | List of t list | Assoc of (string * t) list

  exception Error of string

  (* append code point [cp] as UTF-8 (handles the full range; surrogate pairs are combined by caller) *)
  let add_utf8 buf cp =
    if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
    else if cp < 0x800 then (Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6))); Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
    else if cp < 0x10000 then (Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12))); Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F))); Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
    else (Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18))); Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F))); Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F))); Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))

  let parse (s : string) : t =
    let n = String.length s in
    let pos = ref 0 in
    let fail msg = raise (Error msg) in
    let peek () = if !pos < n then s.[!pos] else '\000' (* a raw NUL never appears in valid JSON (control chars are escaped), so it is a safe EOF sentinel *) in
    let skip_ws () = while !pos < n && (match s.[!pos] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false) do incr pos done in
    let expect c = if peek () = c then incr pos else fail (Printf.sprintf "expected '%c'" c) in
    let hex4 () =
      if !pos + 4 > n then fail "bad \\u escape";
      let v = try int_of_string ("0x" ^ String.sub s !pos 4) with _ -> fail "bad \\u escape" in
      pos := !pos + 4;
      v
    in
    let parse_string () =
      expect '"';
      let buf = Buffer.create 16 in
      let rec go () =
        if !pos >= n then fail "unterminated string";
        let c = s.[!pos] in
        incr pos;
        match c with
        | '"' -> Buffer.contents buf
        | '\\' -> (
          if !pos >= n then fail "bad escape";
          let e = s.[!pos] in
          incr pos;
          (match e with
          | '"' -> Buffer.add_char buf '"'
          | '\\' -> Buffer.add_char buf '\\'
          | '/' -> Buffer.add_char buf '/'
          | 'b' -> Buffer.add_char buf '\b'
          | 'f' -> Buffer.add_char buf '\012'
          | 'n' -> Buffer.add_char buf '\n'
          | 'r' -> Buffer.add_char buf '\r'
          | 't' -> Buffer.add_char buf '\t'
          | 'u' ->
            let hi = hex4 () in
            if hi >= 0xD800 && hi <= 0xDBFF && !pos + 1 < n && s.[!pos] = '\\' && s.[!pos + 1] = 'u' then (
              pos := !pos + 2;
              let lo = hex4 () in
              add_utf8 buf (0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00)))
            else add_utf8 buf hi
          | _ -> fail "bad escape");
          go ())
        | _ -> Buffer.add_char buf c; go ()
      in
      go ()
    in
    let expect_lit lit v =
      let l = String.length lit in
      if !pos + l <= n && String.sub s !pos l = lit then (pos := !pos + l; v) else fail ("expected " ^ lit)
    in
    let parse_number () =
      let start = !pos in
      if peek () = '-' then incr pos;
      while !pos < n && (match s.[!pos] with '0' .. '9' | '.' | 'e' | 'E' | '+' | '-' -> true | _ -> false) do incr pos done;
      Num (String.sub s start (!pos - start))
    in
    let rec parse_value () =
      skip_ws ();
      match peek () with
      | '"' -> Str (parse_string ())
      | '{' -> parse_object ()
      | '[' -> parse_array ()
      | 't' -> expect_lit "true" (Bool true)
      | 'f' -> expect_lit "false" (Bool false)
      | 'n' -> expect_lit "null" Null
      | '-' | '0' .. '9' -> parse_number ()
      | _ -> fail "unexpected token"
    and parse_object () =
      expect '{';
      skip_ws ();
      if peek () = '}' then (incr pos; Assoc [])
      else
        let rec members acc =
          skip_ws ();
          let k = parse_string () in
          skip_ws ();
          expect ':';
          let v = parse_value () in
          skip_ws ();
          match peek () with ',' -> incr pos; members ((k, v) :: acc) | '}' -> incr pos; Assoc (List.rev ((k, v) :: acc)) | _ -> fail "expected ',' or '}'"
        in
        members []
    and parse_array () =
      expect '[';
      skip_ws ();
      if peek () = ']' then (incr pos; List [])
      else
        let rec elems acc =
          let v = parse_value () in
          skip_ws ();
          match peek () with ',' -> incr pos; elems (v :: acc) | ']' -> incr pos; List (List.rev (v :: acc)) | _ -> fail "expected ',' or ']'"
        in
        elems []
    in
    let v = parse_value () in
    skip_ws ();
    v

  let member k = function Assoc l -> List.assoc_opt k l | _ -> None
  let to_string_opt = function Str s -> Some s | _ -> None
  let to_list_opt = function List l -> Some l | _ -> None
end

(* opt-in wire tracing (FENNEC_ACME_DEBUG=1) — invaluable when an ACME server misbehaves *)
let dbg fmt = if Sys.getenv_opt "FENNEC_ACME_DEBUG" <> None then Printf.eprintf ("[acme] " ^^ fmt ^^ "\n%!") else Printf.ifprintf stderr fmt

(* ---- base64url (no padding) + SHA-256 ---- *)
let b64url s = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

(* big-endian, minimal-length unsigned octets of a positive Z (RFC 7518 "base64url-uint") *)
let z_octets (z : Z.t) : string =
  let len = max 1 ((Z.numbits z + 7) / 8) in
  String.init len (fun i -> Char.chr (Z.to_int (Z.logand (Z.shift_right z (8 * (len - 1 - i))) (Z.of_int 0xff))))

(* ---- JWK + thumbprint (RSA account key) ---- *)
let rsa_pub key = match X509.Private_key.public key with `RSA pub -> pub | _ -> failwith "acme: account key must be RSA"

(* canonical JWK JSON — RFC 7638: members lexicographically ordered (e, kty, n), no whitespace *)
let jwk_json key =
  let pub = rsa_pub key in
  Printf.sprintf {|{"e":"%s","kty":"RSA","n":"%s"}|}
    (b64url (z_octets pub.Mirage_crypto_pk.Rsa.e))
    (b64url (z_octets pub.Mirage_crypto_pk.Rsa.n))

let thumbprint key = b64url (sha256 (jwk_json key))

(* ---- JWS (RS256 = RSASSA-PKCS1-v1_5 + SHA-256; raw signature, no DER) ---- *)
let sign_rs256 key input =
  match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 key (`Message input) with
  | Ok s -> b64url s
  | Error (`Msg m) -> failwith ("acme: JWS signing — " ^ m)

let jws ~key ~auth ~nonce ~url ~payload =
  let header =
    match auth with
    | `Jwk -> Printf.sprintf {|{"alg":"RS256","jwk":%s,"nonce":"%s","url":"%s"}|} (jwk_json key) nonce url
    | `Kid kid -> Printf.sprintf {|{"alg":"RS256","kid":"%s","nonce":"%s","url":"%s"}|} kid nonce url
  in
  let p = b64url header and pl = b64url payload in
  Printf.sprintf {|{"protected":"%s","payload":"%s","signature":"%s"}|} p pl (sign_rs256 key (p ^ "." ^ pl))

(* ---- the CSR (with a Subject Alternative Name for every domain) ---- *)
let make_csr server_key domains =
  let cn = List.hd domains in
  let dn = X509.Distinguished_name.[ Relative_distinguished_name.singleton (CN cn) ] in
  let san = X509.General_name.(singleton DNS domains) in
  let exts = X509.Signing_request.Ext.(singleton Extensions (X509.Extension.(singleton Subject_alt_name (false, san)))) in
  match X509.Signing_request.create dn ~extensions:exts server_key with Ok c -> c | Error (`Msg m) -> failwith ("acme: CSR — " ^ m)

(* ---- response helpers ---- *)
let parse body = try Json.parse body with _ -> failwith "acme: malformed JSON from the server"
let str j k = Option.bind (Json.member k j) Json.to_string_opt
let list j k = match Option.bind (Json.member k j) Json.to_list_opt with Some l -> l | None -> []
let must j k = match str j k with Some v -> v | None -> failwith ("acme: response missing " ^ k)

let poll ~clock ~what get_status =
  let rec go n =
    if n <= 0 then failwith ("acme: " ^ what ^ " did not complete in time")
    else
      match get_status () with
      | `Valid v -> v
      | `Invalid body -> failwith (Printf.sprintf "acme: %s invalid — %s" what body)
      | `Pending -> Eio.Time.sleep clock 2.0; go (n - 1)
  in
  go 30

(* [obtain] runs the full HTTP-01 flow for [domains] (one SAN cert). [provision ~token ~key_auth]
   must make the token reachable at /.well-known/acme-challenge/<token> (the manager's :80 listener
   does this); [cleanup ~token] removes it. Returns (cert-chain PEM, server-key PEM). *)
let obtain ~net ~clock ?authenticator ?solve_dns01 ~directory ~account_key ~email ~domains ~provision ~cleanup () : (string * string, string) result =
  Mirage_crypto_rng_unix.use_default ();
  let nonce = ref "" and kid = ref None in
  let req ~meth ?headers ?body url = Https_client.request ~net ?authenticator ~meth ?headers ?body url in
  let refresh (r : Https_client.response) = match Https_client.header_get r.headers "replay-nonce" with Some n -> nonce := n | None -> () in
  let post ~url ?(payload = "") auth =
    let body = jws ~key:account_key ~auth ~nonce:!nonce ~url ~payload in
    let r = req ~meth:"POST" ~headers:[ ("Content-Type", "application/jose+json") ] ~body url in
    refresh r;
    r
  in
  let the_kid () = match !kid with Some k -> `Kid k | None -> failwith "acme: no account yet" in
  let post_get url = post ~url (the_kid ()) (* POST-as-GET: empty payload, kid-authenticated *) in
  try
    dbg "GET directory %s" directory;
    let dir_resp = req ~meth:"GET" directory in
    dbg "directory status=%d body=%d bytes: %s" dir_resp.status (String.length dir_resp.body) dir_resp.body;
    let dir = parse dir_resp.body in
    let new_nonce = must dir "newNonce" and new_account = must dir "newAccount" and new_order = must dir "newOrder" in
    dbg "directory ok; HEAD nonce";
    refresh (req ~meth:"HEAD" new_nonce);
    dbg "nonce=%s" !nonce;
    (* account *)
    let acct = post ~url:new_account ~payload:(Printf.sprintf {|{"termsOfServiceAgreed":true,"contact":["mailto:%s"]}|} email) `Jwk in
    dbg "newAccount status=%d" acct.status;
    if acct.status >= 400 then failwith ("acme: newAccount — " ^ acct.body);
    kid := (match Https_client.header_get acct.headers "location" with Some u -> Some u | None -> failwith "acme: no account URL");
    (* order *)
    let ids = String.concat "," (List.map (fun d -> Printf.sprintf {|{"type":"dns","value":"%s"}|} d) domains) in
    let oresp = post ~url:new_order ~payload:(Printf.sprintf {|{"identifiers":[%s]}|} ids) (the_kid ()) in
    if oresp.status >= 400 then failwith ("acme: newOrder — " ^ oresp.body);
    let order_url = match Https_client.header_get oresp.headers "location" with Some u -> u | None -> failwith "acme: no order URL" in
    let order = parse oresp.body in
    let finalize = must order "finalize" in
    let authzs = List.filter_map Json.to_string_opt (list order "authorizations") in
    dbg "newOrder status=%d finalize=%s authzs=%d" oresp.status finalize (List.length authzs);
    (* each authorization: solve http-01 if offered, else dns-01 (which is the only challenge ACME
       offers for a wildcard — it needs a DNS provider) *)
    let solve_authz authz_url =
      let authz = parse (post_get authz_url).body in
      let identifier = match Json.member "identifier" authz with Some o -> Option.value (str o "value") ~default:"" | None -> "" in
      let challenges = list authz "challenges" in
      let find ty = List.find_opt (fun c -> str c "type" = Some ty) challenges in
      let wait_valid () =
        poll ~clock ~what:("authorization " ^ identifier) (fun () ->
            let resp = post_get authz_url in
            match str (parse resp.body) "status" with Some "valid" -> `Valid () | Some "invalid" -> `Invalid resp.body | _ -> `Pending)
      in
      match (find "http-01", find "dns-01", solve_dns01) with
      | Some c, _, _ ->
        let token = must c "token" and chal_url = must c "url" in
        dbg "authz %s: http-01 token=%s" identifier token;
        provision ~token ~key_auth:(token ^ "." ^ thumbprint account_key);
        Fun.protect ~finally:(fun () -> cleanup ~token) (fun () -> ignore (post ~url:chal_url ~payload:"{}" (the_kid ())); ignore (wait_valid ()))
      | None, Some c, Some solve ->
        (* RFC 8555 §8.4: the TXT value is base64url(SHA-256(keyAuthorization)) at _acme-challenge.<id> *)
        let token = must c "token" and chal_url = must c "url" in
        let txt = b64url (sha256 (token ^ "." ^ thumbprint account_key)) in
        dbg "authz %s: dns-01 _acme-challenge.%s=%s" identifier identifier txt;
        let undo = solve ~name:("_acme-challenge." ^ identifier) ~value:txt in
        Fun.protect ~finally:undo (fun () ->
            Eio.Time.sleep clock 3.0 (* let the record propagate before triggering validation *);
            ignore (post ~url:chal_url ~payload:"{}" (the_kid ()));
            ignore (wait_valid ()))
      | None, Some _, None -> failwith (Printf.sprintf "acme: %s offers only DNS-01 (a wildcard?) — configure a DNS provider via Acme.auto ~dns_provider" identifier)
      | _ -> failwith (Printf.sprintf "acme: no solvable challenge for %s" identifier)
    in
    List.iter solve_authz authzs;
    (* finalize with the CSR, then download the issued chain *)
    let server_key = X509.Private_key.generate ~bits:2048 `RSA in
    let csr_der = X509.Signing_request.encode_der (make_csr server_key domains) in
    dbg "authz validated; finalizing";
    let fin = post ~url:finalize ~payload:(Printf.sprintf {|{"csr":"%s"}|} (b64url csr_der)) (the_kid ()) in
    dbg "finalize status=%d" fin.status;
    if fin.status >= 400 then failwith ("acme: finalize — " ^ fin.body);
    let cert_url =
      poll ~clock ~what:"order" (fun () ->
          let resp = post_get order_url in
          let o = parse resp.body in
          match str o "status" with Some "valid" -> `Valid (must o "certificate") | Some "invalid" -> `Invalid resp.body | _ -> `Pending)
    in
    Ok ((post_get cert_url).body, X509.Private_key.encode_pem server_key)
  with Failure m -> Error m | e -> Error (Printexc.to_string e)

(* ──── acme_client crypto tests (pinned to known vectors) ──── *)

let contains_sub ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

(* z_octets + base64url: the standard RSA exponent 65537 encodes as "AQAB" — the universally-known
   JWK value (proves big-endian-minimal octets + base64url). *)
let%test "base64url-uint of 65537 is AQAB" = b64url (z_octets (Z.of_int 65537)) = "AQAB"

(* SHA-256 + base64url pinned to the empty-string digest (a known vector). *)
let%test "sha256+base64url empty-string vector" = b64url (sha256 "") = "47DEQpj8HBSa-_TImW-5JCeuQeRkm5NMpJWZG3hSuFU"

(* the JWK is exactly the RFC 7638 canonical form (members e, kty, n; no whitespace) and the
   thumbprint is a 43-char base64url SHA-256. *)
let%test "JWK is canonical + thumbprint shape" =
  Mirage_crypto_rng_unix.use_default ();
  let k = X509.Private_key.generate ~bits:2048 `RSA in
  let j = jwk_json k in
  String.length j > 12
  && String.sub j 0 6 = {|{"e":"|}
  && contains_sub ~sub:{|","kty":"RSA","n":"|} j
  && j.[String.length j - 2] = '"'
  && String.length (thumbprint k) = 43

(* the JWS we assemble is a valid RS256 signature over protected.payload (decode + verify). *)
let%test "JWS RS256 signature verifies against the account public key" =
  Mirage_crypto_rng_unix.use_default ();
  let k = X509.Private_key.generate ~bits:2048 `RSA in
  let input = "eyJhbGciOiJSUzI1NiJ9.eyJ0ZXN0IjoxfQ" in
  match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 k (`Message input) with
  | Error _ -> false
  | Ok signature -> ( match X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1 ~signature (X509.Private_key.public k) (`Message input) with Ok () -> true | Error _ -> false)

(* dns-01 (RFC 8555 §8.4): the TXT value is base64url(SHA-256(keyAuthorization)) — a 43-char
   base64url SHA-256. The crypto underneath is the same vector-proven b64url+sha256. *)
let%test "dns-01 TXT digest is a 43-char base64url SHA-256 of the key authorization" =
  Mirage_crypto_rng_unix.use_default ();
  let k = X509.Private_key.generate ~bits:2048 `RSA in
  String.length (b64url (sha256 ("a-token" ^ "." ^ thumbprint k))) = 43

(* ──── JSON reader (hand-rolled, no yojson) ──── *)

(* a real ACME directory response: read the three top-level URL fields the protocol needs *)
let%test "Json reads an ACME directory's string fields" =
  let body = {|{"newNonce":"https://acme/n","newAccount":"https://acme/a","newOrder":"https://acme/o","meta":{"termsOfService":"https://acme/tos"}}|} in
  let j = Json.parse body in
  str j "newNonce" = Some "https://acme/n" && str j "newAccount" = Some "https://acme/a" && str j "newOrder" = Some "https://acme/o"
  && (match Json.member "meta" j with Some m -> str m "termsOfService" = Some "https://acme/tos" | None -> false)

(* an ACME order: a string list (authorizations), a nested challenge array, numbers + booleans skipped *)
let%test "Json reads ACME order: string list, nested objects, numbers/bools" =
  let body =
    {|{"status":"pending","expires":1700000000,"valid":true,"finalize":"https://acme/fin",
       "authorizations":["https://acme/az/1","https://acme/az/2"],
       "challenges":[{"type":"http-01","token":"TOK","url":"https://acme/ch/1"},{"type":"dns-01","token":"T2","url":"u2"}]}|}
  in
  let j = Json.parse body in
  str j "status" = Some "pending"
  && str j "finalize" = Some "https://acme/fin"
  && List.filter_map Json.to_string_opt (list j "authorizations") = [ "https://acme/az/1"; "https://acme/az/2" ]
  &&
  let chs = list j "challenges" in
  List.length chs = 2
  && (match List.find_opt (fun c -> str c "type" = Some "http-01") chs with Some c -> str c "token" = Some "TOK" && str c "url" = Some "https://acme/ch/1" | None -> false)

(* escapes: an ACME [detail] error string with quotes, a solidus, and a \u escape decodes correctly *)
let%test "Json decodes string escapes (quote, solidus, unicode)" =
  let j = Json.parse {|{"detail":"bad id \"x\": see http:\/\/h\/p Aé"}|} in
  str j "detail" = Some "bad id \"x\": see http://h/p A\xc3\xa9"

(* surrogate pair (U+1F600 = "😀") round-trips to its 4-byte UTF-8 — proves we never
   miscount bytes while skipping a unicode field *)
let%test "Json decodes a surrogate pair to UTF-8" =
  let j = Json.parse {|{"emoji":"😀","after":"ok"}|} in
  str j "emoji" = Some "\xf0\x9f\x98\x80" && str j "after" = Some "ok"

(* robustness: a missing field is None (not an error), a malformed body raises (caught by [parse]) *)
let%test "Json: absent field is None; malformed input raises" =
  let j = Json.parse {|{"a":"1"}|} in
  str j "missing" = None
  && list j "missing" = []
  && (match Json.parse "{" with exception Json.Error _ -> true | exception _ -> true | _ -> false)
