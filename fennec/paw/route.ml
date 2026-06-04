(* Route paws — method+path matchers that answer when they match, else decline
   (pass the conn through). These are the [.get]/[.post]/… verbs; each is just a
   paw, so they compose in a pipeline like any other. *)

module H = Fennec_core.Http

(* HEAD is matched as GET (the responder strips the body downstream) *)
let meth_matches (want : H.meth) (got : H.meth) =
  got = want || (want = H.GET && got = H.HEAD)

(* path -> non-empty segments (a trailing/leading/double slash is ignored) *)
let segments (s : string) : string list =
  String.split_on_char '/' s |> List.filter (fun x -> x <> "")

(* match a pattern with [:name] (one segment) and a trailing [*name] (the rest) against a
   path, returning the captured params, or [None] if it doesn't match *)
let match_pattern (pattern : string) (path : string) : (string * string) list option =
  let rec go ps xs acc =
    match (ps, xs) with
    | [], [] -> Some (List.rev acc)
    | [ p ], _ when String.length p > 0 && p.[0] = '*' ->
      Some (List.rev ((String.sub p 1 (String.length p - 1), String.concat "/" xs) :: acc))
    | p :: ps', x :: xs' ->
      if String.length p > 0 && p.[0] = ':' then
        go ps' xs' ((String.sub p 1 (String.length p - 1), x) :: acc)
      else if p = x then go ps' xs' acc
      else None
    | _ -> None
  in
  go (segments pattern) (segments path) []

let has_params (pattern : string) = String.contains pattern ':' || String.contains pattern '*'

(* a method+path route; [h] is run when it matches. A pattern with [:name]/[*name]
   captures path params onto the conn (read with {!Conn.path_param}); a plain pattern is an
   exact string match. *)
let on (m : H.meth) (pattern : string) (h : Paw.t) : Paw.t =
 fun c ->
  if not (meth_matches m (Conn.meth c)) then c
  else if has_params pattern then
    match match_pattern pattern (Conn.path c) with Some ps -> h (Conn.set_path_params c ps) | None -> c
  else if Conn.path c = pattern then h c
  else c

let get path h = on H.GET path h
let post path h = on H.POST path h
let put path h = on H.PUT path h
let delete path h = on H.DELETE path h
let patch path h = on H.PATCH path h

(* a fallthrough paw from a [request -> response option] (e.g. static files):
   answers when it yields Some, else declines *)
let fallthrough (f : H.request -> H.response option) : Paw.t =
 fun c -> match f (Conn.req c) with Some r -> Conn.respond c r | None -> c
