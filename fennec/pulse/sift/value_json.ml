(* value_json.ml — native relaxed JSON parser into the neutral {!Value} model. NO Bson, NO Json.t AST:
   a self-contained recursive-descent parser over the eight-constructor data model, so JSON decode is
   a first-class Sift format that never touches BSON. Relaxed/plain JSON (RFC 8259) — strings, numbers,
   true/false/null, arrays, objects — the wire form an HTTP API speaks (distinct from the bson_json
   bridge's CANONICAL extended JSON, $oid/$date, which is BSON-interchange).

   A JSON integer becomes [Int], a fractional/exponent number [Float]. Pairs with {!Sift.decode_json}
   (= parse + {!Sift.of_value}): reads back what {!Sift.encode_json} emits — an id as a string, a date
   as a number, which of_value's coercions accept. (Encode stays the shape-direct {!Json_writer}.) *)

exception Parse_error of string

let of_string (s : string) : (Value.t, string) result =
  let n = String.length s in
  let pos = ref 0 in
  let error msg = raise (Parse_error msg) in
  let is_digit c = c >= '0' && c <= '9' in
  let rec skip_ws () = if !pos < n then ( match s.[!pos] with ' ' | '\t' | '\n' | '\r' -> incr pos; skip_ws () | _ -> ()) in
  let expect lit =
    let ln = String.length lit in
    if !pos + ln <= n && String.sub s !pos ln = lit then pos := !pos + ln else error (Printf.sprintf "expected %s" lit)
  in
  let hex4 () =
    if !pos + 4 > n then error "truncated \\u escape";
    let h = String.sub s !pos 4 in
    pos := !pos + 4;
    match int_of_string_opt ("0x" ^ h) with Some cp -> cp | None -> error "invalid \\u escape"
  in
  let utf8 buf cp =
    if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
    else if cp < 0x800 then (Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6))); Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
    else if cp < 0x10000 then (
      Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
    else (
      Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
  in
  let parse_string () =
    incr pos; (* opening quote *)
    let buf = Buffer.create 16 in
    let rec loop () =
      if !pos >= n then error "unterminated string";
      match s.[!pos] with
      | '"' -> incr pos; Buffer.contents buf
      | '\\' ->
          incr pos;
          if !pos >= n then error "unterminated escape";
          (match s.[!pos] with
          | '"' -> Buffer.add_char buf '"'; incr pos
          | '\\' -> Buffer.add_char buf '\\'; incr pos
          | '/' -> Buffer.add_char buf '/'; incr pos
          | 'b' -> Buffer.add_char buf '\b'; incr pos
          | 'f' -> Buffer.add_char buf '\012'; incr pos
          | 'n' -> Buffer.add_char buf '\n'; incr pos
          | 'r' -> Buffer.add_char buf '\r'; incr pos
          | 't' -> Buffer.add_char buf '\t'; incr pos
          | 'u' ->
              incr pos;
              let cp = hex4 () in
              let cp =
                if cp >= 0xD800 && cp <= 0xDBFF then
                  if !pos + 2 <= n && s.[!pos] = '\\' && s.[!pos + 1] = 'u' then (
                    pos := !pos + 2;
                    let lo = hex4 () in
                    if lo >= 0xDC00 && lo <= 0xDFFF then 0x10000 + ((cp - 0xD800) lsl 10) + (lo - 0xDC00) else error "invalid low surrogate")
                  else error "expected low surrogate"
                else cp
              in
              utf8 buf cp
          | c -> error (Printf.sprintf "invalid escape \\%c" c));
          loop ()
      | c -> Buffer.add_char buf c; incr pos; loop ()
    in
    loop ()
  in
  let parse_number () =
    let start = !pos in
    if !pos < n && s.[!pos] = '-' then incr pos;
    while !pos < n && is_digit s.[!pos] do incr pos done;
    let is_float = ref false in
    if !pos < n && s.[!pos] = '.' then (is_float := true; incr pos; while !pos < n && is_digit s.[!pos] do incr pos done);
    if !pos < n && (s.[!pos] = 'e' || s.[!pos] = 'E') then (
      is_float := true;
      incr pos;
      if !pos < n && (s.[!pos] = '+' || s.[!pos] = '-') then incr pos;
      while !pos < n && is_digit s.[!pos] do incr pos done);
    let lit = String.sub s start (!pos - start) in
    if !is_float then ( match float_of_string_opt lit with Some f -> Value.Float f | None -> error "invalid number")
    else
      match int_of_string_opt lit with
      | Some i -> Value.Int i (* a non-fractional number too big for a 63-bit int falls back to float *)
      | None -> ( match float_of_string_opt lit with Some f -> Value.Float f | None -> error "invalid number")
  in
  let rec parse_value () =
    skip_ws ();
    if !pos >= n then error "unexpected end of input";
    match s.[!pos] with
    | '{' -> parse_object ()
    | '[' -> parse_array ()
    | '"' -> Value.String (parse_string ())
    | 't' -> expect "true"; Value.Bool true
    | 'f' -> expect "false"; Value.Bool false
    | 'n' -> expect "null"; Value.Null
    | '-' -> parse_number ()
    | c when is_digit c -> parse_number ()
    | c -> error (Printf.sprintf "unexpected character %C" c)
  and parse_array () =
    incr pos; (* [ *)
    skip_ws ();
    if !pos < n && s.[!pos] = ']' then (incr pos; Value.List [])
    else
      let rec loop acc =
        let v = parse_value () in
        skip_ws ();
        if !pos >= n then error "unterminated array";
        match s.[!pos] with
        | ',' -> incr pos; loop (v :: acc)
        | ']' -> incr pos; Value.List (List.rev (v :: acc))
        | _ -> error "expected ',' or ']' in array"
      in
      loop []
  and parse_object () =
    incr pos; (* { *)
    skip_ws ();
    if !pos < n && s.[!pos] = '}' then (incr pos; Value.Assoc [])
    else
      let rec loop acc =
        skip_ws ();
        if !pos >= n || s.[!pos] <> '"' then error "expected string key in object";
        let k = parse_string () in
        skip_ws ();
        if !pos >= n || s.[!pos] <> ':' then error "expected ':' after key";
        incr pos;
        let v = parse_value () in
        skip_ws ();
        if !pos >= n then error "unterminated object";
        match s.[!pos] with
        | ',' -> incr pos; loop ((k, v) :: acc)
        | '}' -> incr pos; Value.Assoc (List.rev ((k, v) :: acc))
        | _ -> error "expected ',' or '}' in object"
      in
      loop []
  in
  try
    let v = parse_value () in
    skip_ws ();
    if !pos <> n then error "trailing data after JSON value";
    Ok v
  with Parse_error m -> Error m
