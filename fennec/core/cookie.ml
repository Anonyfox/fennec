(* HTTP cookies (RFC 6265) — pure parsing + Set-Cookie serialization. The request
   [Cookie] header carries [name=value] pairs separated by "; "; a response sets one
   [Set-Cookie] header per cookie (they must NOT be folded into one line). Values are
   stored/emitted verbatim — encoding is the caller's choice. *)

type same_site = Strict | Lax | None_

let same_site_to_string = function Strict -> "Strict" | Lax -> "Lax" | None_ -> "None"

(* parse a request [Cookie] header value into name/value pairs (surrounding quotes
   on a value are stripped; whitespace trimmed) *)
let parse_header (h : string) : (string * string) list =
  String.split_on_char ';' h
  |> List.filter_map (fun part ->
         let part = String.trim part in
         if part = "" then None
         else
           match String.index_opt part '=' with
           | Some i ->
             let k = String.trim (String.sub part 0 i) in
             let v = String.trim (String.sub part (i + 1) (String.length part - i - 1)) in
             let v =
               if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"' then
                 String.sub v 1 (String.length v - 2)
               else v
             in
             if k = "" then None else Some (k, v)
           | None -> Some (part, ""))

(* serialize one [Set-Cookie] header value. [path] defaults to "/", [http_only] to
   true and [same_site] to [Lax] (modern-safe defaults); [SameSite=None] implies
   [Secure] per the spec. *)
let to_set_cookie ~name ~value ?(path = "/") ?domain ?max_age ?expires ?(secure = false)
    ?(http_only = true) ?(same_site = Lax) () : string =
  let b = Buffer.create 64 in
  Buffer.add_string b name;
  Buffer.add_char b '=';
  Buffer.add_string b value;
  (if path <> "" then Buffer.add_string b ("; Path=" ^ path));
  (match domain with Some d -> Buffer.add_string b ("; Domain=" ^ d) | None -> ());
  (match max_age with Some s -> Buffer.add_string b (Printf.sprintf "; Max-Age=%d" s) | None -> ());
  (match expires with Some t -> Buffer.add_string b ("; Expires=" ^ Http_date.format t) | None -> ());
  Buffer.add_string b ("; SameSite=" ^ same_site_to_string same_site);
  if secure || same_site = None_ then Buffer.add_string b "; Secure";
  if http_only then Buffer.add_string b "; HttpOnly";
  Buffer.contents b
