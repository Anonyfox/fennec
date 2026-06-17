(* The pure path + method matching rules, shared by the runtime route-as-paw constructor
   ({!Pipeline.on}) and the compiled dispatch table ({!Route_table}). No Conn, no Pipeline — just
   strings + methods — so the opaque and the compiled routers apply byte-for-byte identical semantics. *)

module H = Http

(* HEAD is satisfied by a GET route (the responder strips the body downstream) *)
let meth_matches (want : H.meth) (got : H.meth) : bool = got = want || (want = H.GET && got = H.HEAD)

(* path -> non-empty segments (a leading/trailing/double slash is ignored) *)
let segments (s : string) : string list = String.split_on_char '/' s |> List.filter (fun x -> x <> "")

(* match pre-split pattern segments against pre-split path segments, capturing [:name] (one segment)
   and a trailing [*name] (the rest); [None] if they don't match *)
let rec match_segments ps xs acc =
  match (ps, xs) with
  | [], [] -> Some (List.rev acc)
  | [ p ], _ when String.length p > 0 && p.[0] = '*' -> Some (List.rev ((String.sub p 1 (String.length p - 1), String.concat "/" xs) :: acc))
  | p :: ps', x :: xs' ->
    if String.length p > 0 && p.[0] = ':' then match_segments ps' xs' ((String.sub p 1 (String.length p - 1), x) :: acc)
    else if p = x then match_segments ps' xs' acc
    else None
  | _ -> None

(* does the pattern carry a [:param] or [*splat]? (so it needs segment matching, not a string compare) *)
let has_params (pattern : string) : bool = String.contains pattern ':' || String.contains pattern '*'

(* ──── route_match ──── *)

let segs_ = [ "users"; ":id" ]
let%test "static match" = match_segments [ "a"; "b" ] [ "a"; "b" ] [] = Some []
let%test "static mismatch" = match_segments [ "a"; "b" ] [ "a"; "c" ] [] = None
let%test "param capture" = match_segments segs_ [ "users"; "42" ] [] = Some [ ("id", "42") ]
let%test "splat captures rest" = match_segments [ "files"; "*rest" ] [ "files"; "a"; "b" ] [] = Some [ ("rest", "a/b") ]
let%test "length mismatch" = match_segments [ "a" ] [ "a"; "b" ] [] = None
let%test "has_params :" = has_params "/users/:id"
let%test "has_params *" = has_params "/files/*rest"
let%test "no params" = not (has_params "/users/me")
let%test "head matches get" = meth_matches H.GET H.HEAD
let%test "get not post" = not (meth_matches H.GET H.POST)
