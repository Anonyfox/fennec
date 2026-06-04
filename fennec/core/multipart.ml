(* multipart/form-data parsing (RFC 7578) — pure. A request body of this type is a
   sequence of parts separated by a boundary; each part has its own headers
   (Content-Disposition gives the field [name] and, for files, a [filename]) and a
   raw payload. Used for HTML form submissions that include file uploads. *)

type part = {
  name : string;             (* the form field name *)
  filename : string option;  (* present for file parts *)
  content_type : string;     (* the part's Content-Type (default text/plain) *)
  data : string;             (* the raw payload bytes *)
}

(* find [sep] in [s] at or after [from]; index or -1 *)
let find_from (s : string) (sep : string) (from : int) : int =
  let n = String.length s and m = String.length sep in
  if m = 0 then from
  else begin
    let rec go i = if i + m > n then -1 else if String.sub s i m = sep then i else go (i + 1) in
    go from
  end

(* split [s] on every occurrence of [sep] *)
let split_on (s : string) (sep : string) : string list =
  let m = String.length sep in
  let rec go start acc =
    match find_from s sep start with
    | -1 -> List.rev (String.sub s start (String.length s - start) :: acc)
    | i -> go (i + m) (String.sub s start (i - start) :: acc)
  in
  if m = 0 then [ s ] else go 0 []

let strip_leading_crlf s =
  if String.length s >= 2 && s.[0] = '\r' && s.[1] = '\n' then String.sub s 2 (String.length s - 2) else s

let strip_trailing_crlf s =
  let n = String.length s in
  if n >= 2 && s.[n - 2] = '\r' && s.[n - 1] = '\n' then String.sub s 0 (n - 2) else s

(* the boundary token from a multipart Content-Type, e.g. "...; boundary=abc" *)
let boundary_of_content_type (ct : string) : string option =
  let parts = String.split_on_char ';' ct in
  List.find_map
    (fun p ->
      let p = String.trim p in
      match String.index_opt p '=' with
      | Some i when String.trim (String.sub p 0 i) = "boundary" ->
        let v = String.trim (String.sub p (i + 1) (String.length p - i - 1)) in
        let v = if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"'
                then String.sub v 1 (String.length v - 2) else v in
        if v = "" then None else Some v
      | _ -> None)
    parts

(* parse a Content-Disposition value: name + optional filename *)
let parse_disposition (v : string) : string option * string option =
  let unq s = if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'
              then String.sub s 1 (String.length s - 2) else s in
  let attr key =
    List.find_map
      (fun p ->
        let p = String.trim p in
        match String.index_opt p '=' with
        | Some i when String.trim (String.sub p 0 i) = key ->
          Some (unq (String.trim (String.sub p (i + 1) (String.length p - i - 1))))
        | _ -> None)
      (String.split_on_char ';' v)
  in
  (attr "name", attr "filename")

let parse_part (chunk : string) : part option =
  let chunk = strip_leading_crlf chunk in
  match find_from chunk "\r\n\r\n" 0 with
  | -1 -> None
  | i ->
    let raw_headers = String.sub chunk 0 i in
    let data = strip_trailing_crlf (String.sub chunk (i + 4) (String.length chunk - i - 4)) in
    let headers =
      String.split_on_char '\n' raw_headers
      |> List.filter_map (fun line ->
             let line = (if line <> "" && line.[String.length line - 1] = '\r'
                         then String.sub line 0 (String.length line - 1) else line) in
             match String.index_opt line ':' with
             | Some j ->
               Some (String.trim (String.sub line 0 j),
                     String.trim (String.sub line (j + 1) (String.length line - j - 1)))
             | None -> None)
    in
    let disp = Headers.get headers "content-disposition" in
    let name, filename = match disp with Some v -> parse_disposition v | None -> (None, None) in
    (match name with
     | Some name ->
       Some { name; filename;
              content_type = Option.value (Headers.get headers "content-type") ~default:"text/plain";
              data }
     | None -> None)

(* parse a multipart/form-data [body] given its [boundary] *)
let parse ~(boundary : string) (body : string) : part list =
  split_on body ("--" ^ boundary)
  |> List.filter_map (fun chunk ->
         (* skip the preamble ("") and the closing delimiter (starts with "--") *)
         if chunk = "" || (String.length chunk >= 2 && chunk.[0] = '-' && chunk.[1] = '-') then None
         else parse_part chunk)
