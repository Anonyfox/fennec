type t = { name : string option; email : string }

let v ?name email =
  let name = match name with Some n when String.trim n <> "" -> Some (String.trim n) | _ -> None in
  { name; email = String.trim email }

let of_string s =
  let s = String.trim s in
  match String.rindex_opt s '<' with
  | Some i -> (
    match String.index_from_opt s i '>' with
    | Some j ->
      let email = String.trim (String.sub s (i + 1) (j - i - 1)) in
      let name = String.trim (String.sub s 0 i) in
      v ?name:(if name = "" then None else Some name) email
    | None -> v s)
  | None -> v s

let email t = t.email

let is_ascii_printable s = String.for_all (fun c -> let n = Char.code c in n >= 0x20 && n < 0x7f) s

(* RFC 5322 "specials" — their presence in a display-name phrase forces a quoted-string *)
let has_specials s = String.exists (fun c -> String.contains "()<>[]:;@\\,.\"" c) s

(* RFC 2047 'B' (base64) encoded-word — used for a non-ASCII display name so the header stays 7-bit *)
let encoded_word s = "=?UTF-8?B?" ^ Base64.encode_string s ^ "?="

(* a quoted-string: wrap in DQUOTE, backslash-escaping DQUOTE and backslash *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (fun c -> if c = '"' || c = '\\' then Buffer.add_char b '\\'; Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

let encode_name name =
  if not (is_ascii_printable name) then encoded_word name
  else if has_specials name then quote name
  else name

let to_header t = match t.name with None -> t.email | Some n -> encode_name n ^ " <" ^ t.email ^ ">"

let header_list addrs = String.concat ", " (List.map to_header addrs)

(* ---- inline tests ---- *)

let%test "bare address has no name and round-trips through the envelope" =
  let a = v "ada@example.com" in
  a.name = None && email a = "ada@example.com" && to_header a = "ada@example.com"

let%test "named address renders as a 5322 phrase" =
  to_header (v ~name:"Ada Lovelace" "ada@example.com") = "Ada Lovelace <ada@example.com>"

let%test "a name with specials is quoted; a non-ASCII name is RFC-2047 encoded" =
  to_header (v ~name:"Lovelace, Ada" "ada@x.com") = "\"Lovelace, Ada\" <ada@x.com>"
  && to_header (v ~name:"Adèle" "a@x.com") = "=?UTF-8?B?" ^ Base64.encode_string "Adèle" ^ "?= <a@x.com>"

let%test "of_string parses both forms and ignores surrounding space" =
  let a = of_string "  Ada <ada@example.com> " and b = of_string "bob@example.com" in
  a.name = Some "Ada" && a.email = "ada@example.com" && b.name = None && b.email = "bob@example.com"
