(* Case-insensitive header operations over a plain assoc list. HTTP field names are
   case-insensitive (RFC 9110 §5.1); we keep the simple [(name, value) list]
   representation (header counts are small, so a list is cache-friendly and beats a
   hashtable) but compare names without allocating a lowercased copy per lookup. *)

type t = (string * string) list

(* allocation-free case-insensitive equality *)
let ci_equal (a : string) (b : string) : bool =
  let n = String.length a in
  n = String.length b
  &&
  let rec go i =
    i = n
    || (Char.lowercase_ascii (String.unsafe_get a i) = Char.lowercase_ascii (String.unsafe_get b i)
       && go (i + 1))
  in
  go 0

(* the first value bound to [name] (the common case) *)
let get (h : t) (name : string) : string option =
  let rec go = function
    | [] -> None
    | (k, v) :: rest -> if ci_equal k name then Some v else go rest
  in
  go h

(* every value bound to [name], in order — for repeatable fields (Set-Cookie, etc.) *)
let get_all (h : t) (name : string) : string list =
  List.filter_map (fun (k, v) -> if ci_equal k name then Some v else None) h

let mem (h : t) (name : string) : bool = get h name <> None

(* remove every binding for [name] *)
let delete (h : t) (name : string) : t = List.filter (fun (k, _) -> not (ci_equal k name)) h

(* set [name] to a single [value], replacing any existing binding(s) *)
let put (h : t) (name : string) (value : string) : t = (name, value) :: delete h name

(* append a binding, keeping any existing one (for repeatable fields) *)
let add (h : t) (name : string) (value : string) : t = h @ [ (name, value) ]
