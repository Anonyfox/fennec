(* Pure path-pattern matcher — compiles to BOTH targets (Stdlib only), so the SAME
   route matching runs on the server and the client. Patterns:

     "/"                exact
     "/about"           exact
     "/users/:id"       ":id" captures one segment -> ("id", value) in params
     "/files/*"         "*" (last segment) captures the rest as ("*", rest)

   Match returns the captured params (possibly empty) or None. *)

type params = (string * string) list

(* split a path into non-empty segments: "/users/42" -> ["users";"42"]; "/" -> [] *)
let segments (p : string) : string list =
  String.split_on_char '/' p |> List.filter (fun s -> s <> "")

(* match [pattern] against [path], returning captured params *)
let match_one ~(pattern : string) (path : string) : params option =
  let ps = segments pattern and xs = segments path in
  let rec go ps xs acc =
    match (ps, xs) with
    | [], [] -> Some (List.rev acc)
    | [ "*" ], rest -> Some (List.rev (("*", String.concat "/" rest) :: acc)) (* greedy tail *)
    | pseg :: ptl, xseg :: xtl ->
      if String.length pseg > 0 && pseg.[0] = ':' then
        let name = String.sub pseg 1 (String.length pseg - 1) in
        go ptl xtl ((name, xseg) :: acc)
      else if pseg = xseg then go ptl xtl acc
      else None
    | _ -> None
  in
  go ps xs []

(* find the first matching (pattern, value) in a route table *)
let find (routes : (string * 'a) list) (path : string) : ('a * params) option =
  let rec go = function
    | [] -> None
    | (pattern, v) :: rest -> (
      match match_one ~pattern path with Some params -> Some (v, params) | None -> go rest)
  in
  go routes

let param (params : params) (name : string) : string option = List.assoc_opt name params
