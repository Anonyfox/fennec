(* A tiny, pure JSON value + parser + serializer. Pure OCaml string processing, so the WHOLE DDP
   protocol layer (EJSON codec, message codec) is shared between native and JavaScript with no
   Yojson/Js.Json target split. UTF-8 is passed through verbatim on output (only the quote,
   backslash and control chars are escaped); backslash-u escapes on input are decoded to UTF-8
   (including surrogate pairs). Numbers round-trip as integers when integral. Sized for protocol
   frames. *)

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of t list
  | Obj of (string * t) list

(* ---- serialize ----------------------------------------------------------- *)

let escape_string (buf : Buffer.t) (s : string) =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c (* UTF-8 bytes pass through unescaped *))
    s;
  Buffer.add_char buf '"'

let number_to_string (f : float) : string =
  if Float.is_integer f && Float.abs f < 1e15 then string_of_int (int_of_float f)
  else
    (* shortest round-trippable-ish form; protocol numbers are small/simple *)
    let s = Printf.sprintf "%.17g" f in
    s

let rec write (buf : Buffer.t) (j : t) =
  match j with
  | Null -> Buffer.add_string buf "null"
  | Bool true -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | Number f -> Buffer.add_string buf (number_to_string f)
  | String s -> escape_string buf s
  | List xs ->
      Buffer.add_char buf '[';
      List.iteri (fun i x -> if i > 0 then Buffer.add_char buf ','; write buf x) xs;
      Buffer.add_char buf ']'
  | Obj kvs ->
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          escape_string buf k;
          Buffer.add_char buf ':';
          write buf v)
        kvs;
      Buffer.add_char buf '}'

let to_string (j : t) : string =
  let buf = Buffer.create 256 in
  write buf j;
  Buffer.contents buf

(* ---- parse --------------------------------------------------------------- *)

exception Parse_error of string

let parse (s : string) : t =
  let n = String.length s in
  let pos = ref 0 in
  let fail msg = raise (Parse_error (Printf.sprintf "%s at %d" msg !pos)) in
  let peek () = if !pos < n then s.[!pos] else '\000' in
  let adv () = incr pos in
  let rec skip_ws () =
    if !pos < n then
      match s.[!pos] with ' ' | '\t' | '\n' | '\r' -> adv (); skip_ws () | _ -> ()
  in
  let expect c = if peek () = c then adv () else fail (Printf.sprintf "expected %c" c) in
  (* encode a Unicode code point as UTF-8 into buf *)
  let add_utf8 buf cp =
    if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
    else if cp < 0x800 then (
      Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
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
  let hex4 () =
    let v = ref 0 in
    for _ = 1 to 4 do
      let c = peek () in
      let d =
        match c with
        | '0' .. '9' -> Char.code c - Char.code '0'
        | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
        | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
        | _ -> fail "bad \\u escape"
      in
      v := (!v * 16) + d;
      adv ()
    done;
    !v
  in
  let parse_string () =
    expect '"';
    let buf = Buffer.create 32 in
    let rec loop () =
      if !pos >= n then fail "unterminated string";
      let c = s.[!pos] in
      adv ();
      match c with
      | '"' -> ()
      | '\\' -> (
          let e = peek () in
          adv ();
          match e with
          | '"' -> Buffer.add_char buf '"'; loop ()
          | '\\' -> Buffer.add_char buf '\\'; loop ()
          | '/' -> Buffer.add_char buf '/'; loop ()
          | 'n' -> Buffer.add_char buf '\n'; loop ()
          | 'r' -> Buffer.add_char buf '\r'; loop ()
          | 't' -> Buffer.add_char buf '\t'; loop ()
          | 'b' -> Buffer.add_char buf '\b'; loop ()
          | 'f' -> Buffer.add_char buf '\012'; loop ()
          | 'u' ->
              let cp = hex4 () in
              let cp =
                if cp >= 0xD800 && cp <= 0xDBFF then
                  (* high surrogate; expect \uXXXX low surrogate *)
                  if peek () = '\\' then (
                    adv ();
                    if peek () = 'u' then (
                      adv ();
                      let lo = hex4 () in
                      0x10000 + ((cp - 0xD800) lsl 10) + (lo - 0xDC00))
                    else fail "expected low surrogate")
                  else fail "expected low surrogate"
                else cp
              in
              add_utf8 buf cp;
              loop ()
          | _ -> fail "bad escape")
      | c -> Buffer.add_char buf c; loop ()
    in
    loop ();
    Buffer.contents buf
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | '"' -> String (parse_string ())
    | '{' -> parse_obj ()
    | '[' -> parse_arr ()
    | 't' -> lit "true" (Bool true)
    | 'f' -> lit "false" (Bool false)
    | 'n' -> lit "null" Null
    | c when c = '-' || (c >= '0' && c <= '9') -> parse_number ()
    | _ -> fail "unexpected character"
  and lit word v =
    let len = String.length word in
    if !pos + len <= n && String.sub s !pos len = word then (pos := !pos + len; v)
    else fail ("expected " ^ word)
  and parse_number () =
    let start = !pos in
    if peek () = '-' then adv ();
    while (match peek () with '0' .. '9' -> true | _ -> false) do adv () done;
    if peek () = '.' then (
      adv ();
      while (match peek () with '0' .. '9' -> true | _ -> false) do adv () done);
    (match peek () with
    | 'e' | 'E' ->
        adv ();
        (match peek () with '+' | '-' -> adv () | _ -> ());
        while (match peek () with '0' .. '9' -> true | _ -> false) do adv () done
    | _ -> ());
    Number (float_of_string (String.sub s start (!pos - start)))
  and parse_arr () =
    expect '[';
    skip_ws ();
    if peek () = ']' then (adv (); List [])
    else (
      let acc = ref [] in
      let rec loop () =
        let v = parse_value () in
        acc := v :: !acc;
        skip_ws ();
        match peek () with
        | ',' -> adv (); loop ()
        | ']' -> adv ()
        | _ -> fail "expected , or ]"
      in
      loop ();
      List (List.rev !acc))
  and parse_obj () =
    expect '{';
    skip_ws ();
    if peek () = '}' then (adv (); Obj [])
    else (
      let acc = ref [] in
      let rec loop () =
        skip_ws ();
        let k = parse_string () in
        skip_ws ();
        expect ':';
        let v = parse_value () in
        acc := (k, v) :: !acc;
        skip_ws ();
        match peek () with
        | ',' -> adv (); loop ()
        | '}' -> adv ()
        | _ -> fail "expected , or }"
      in
      loop ();
      Obj (List.rev !acc))
  in
  let v = parse_value () in
  skip_ws ();
  v

(* ---- small accessors used by the codecs ---------------------------------- *)

let member (k : string) (j : t) : t option =
  match j with Obj kvs -> List.assoc_opt k kvs | _ -> None

let to_string_opt = function String s -> Some s | _ -> None
let to_list_opt = function List xs -> Some xs | _ -> None
