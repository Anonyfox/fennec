(* Case-insensitive header operations over a plain assoc list. HTTP field names are
   case-insensitive (RFC 9110 §5.1); we keep the simple [(name, value) list]
   representation (header counts are small, so a list is cache-friendly and beats a
   hashtable) but compare names without allocating a lowercased copy per lookup. *)

type t = (string * string) list

(* ──── ci_equal ──── *)

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

let%test "ci_equal same case"  = ci_equal "Content-Type" "Content-Type"
let%test "ci_equal diff case"  = ci_equal "content-type" "Content-Type"
let%test "ci_equal diff len"   = not (ci_equal "host" "hosts")
let%test "ci_equal diff content" = not (ci_equal "X-A" "X-B")

(* ──── get ──── *)

(* the first value bound to [name] (the common case) *)
let get (h : t) (name : string) : string option =
  let rec go = function
    | [] -> None
    | (k, v) :: rest -> if ci_equal k name then Some v else go rest
  in
  go h

let%test "get ci"              = get [("Content-Type","text/html")] "content-type" = Some "text/html"
let%test "get first of multi"  = get [("X-A","1");("X-A","2")] "x-a" = Some "1"
let%test "get absent"          = get [("X-Foo","1")] "X-Bar" = None

(* ──── get_all ──── *)

(* every value bound to [name], in order — for repeatable fields (Set-Cookie, etc.) *)
let get_all (h : t) (name : string) : string list =
  List.filter_map (fun (k, v) -> if ci_equal k name then Some v else None) h

let%test "get_all multi"       = get_all [("Set-Cookie","a"); ("set-cookie","b")] "Set-Cookie" = ["a";"b"]

(* ──── mem ──── *)

let mem (h : t) (name : string) : bool = get h name <> None

let%test "mem ci"              = mem [("Host","x")] "host"
let%test "not mem"             = not (mem [("Host","x")] "other")

(* ──── delete ──── *)

(* remove every binding for [name] *)
let delete (h : t) (name : string) : t = List.filter (fun (k, _) -> not (ci_equal k name)) h

let%test "delete removes all"  = delete [("X","1");("x","2");("Y","3")] "X" = [("Y","3")]

(* ──── put ──── *)

(* set [name] to a single [value], replacing any existing binding(s) *)
let put (h : t) (name : string) (value : string) : t = (name, value) :: delete h name

let%test "put replaces"        = put [("X","old")] "X" "new" = [("X","new")]

(* ──── add ──── *)

(* append a binding, keeping any existing one (for repeatable fields) *)
let add (h : t) (name : string) (value : string) : t = h @ [ (name, value) ]

let%test "add appends"         = add [("X","1")] "X" "2" = [("X","1");("X","2")]

(* ──── algebraic laws (properties over arbitrary header lists) ──── *)

(* One property over a SMALL, realistic header list (a few short printable names/values), checking
   the get/put/delete/case laws together — subsumes the old hand-built shared-multi-list tests.
   NB: a naive type-driven [(string*string) list] generator is correct but its deeply-nested shrink
   trees cost ~5s/property — too slow for the fast unit gate — so the sizes are bounded here. *)
let%prop "get/put/delete obey their algebraic laws" =
  let open Fennec_hunt_prop in
  let s = Gen.(string_size ~gen:char_printable (int_range 0 8)) in
  forall
    ~print:(fun (h, k, v) -> Printf.sprintf "(%d entries, k=%S, v=%S)" (List.length h) k v)
    Gen.(triple (list_size (int_range 0 6) (pair s s)) s s)
    (fun (h, k, v) ->
      get (put h k v) k = Some v                     (* put then get the name → that value *)
      && get_all (put h k v) k = [ v ]               (* put leaves exactly one binding *)
      && get_all (delete h k) k = []                 (* delete removes every binding *)
      && get h k = get h (String.uppercase_ascii k)) (* name lookup is case-insensitive *)

let%test "ci_equal mixed case" = ci_equal "Content-Type" "content-TYPE"
let%test "add keeps insertion order" = add [ ("X", "1") ] "X" "2" = [ ("X", "1"); ("X", "2") ]
