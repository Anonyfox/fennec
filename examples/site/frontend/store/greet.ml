(* The /greet page's cross-stage payload — the server computes it, seeds it, the client view decodes
   it with the SAME codec. Plain shared model; lives in the store so server + client bundle agree. *)
type t = { who : string; count : int }
[@@deriving model]

let empty = { who = "?"; count = 0 }
