(* Typed request-scoped storage for a connection — Phoenix's conn.assigns, but
   TYPE-SAFE (this is why fennec is plug-INSPIRED, not a plug copy). A [key]
   carries the type of its value via [Type.Id] (stdlib, zero deps); [get]/[set]
   are checked against the key's type, so there are no casts and no [Obj.magic].

     let current_user : User.t Assigns.key = Assigns.key "current_user"
     let t = Assigns.set t current_user user        (* : 'a key -> 'a -> t  *)
     match Assigns.get t current_user with …         (* : 'a key -> 'a option *)

   The map itself is heterogeneous; each access is statically typed. *)

type 'a key = { id : 'a Type.Id.t; name : string }

(* a stored binding pairs a key id with its value, existentially *)
type binding = B : 'a Type.Id.t * 'a -> binding

type t = binding list (* small per-request; assoc-style is fine and order-stable *)

let empty : t = []

(* mint a fresh typed key. [name] is for debugging/printing only; identity is the
   Type.Id, so two keys with the same name are still distinct. *)
let key (name : string) : 'a key = { id = Type.Id.make (); name }

let name (k : 'a key) : string = k.name

(* set/replace the binding for [k] *)
let set (t : t) (k : 'a key) (v : 'a) : t =
  let without =
    List.filter (fun (B (id, _)) -> Option.is_none (Type.Id.provably_equal id k.id)) t
  in
  B (k.id, v) :: without

(* match one binding against key [k], returning the value at type ['a] when the
   ids provably match. Annotated [type a b] so the [Equal] witness can refine the
   existential [b] to the requested [a] within this scope. *)
let match_binding : type a b. b Type.Id.t -> b -> a key -> a option =
 fun id v k ->
  match Type.Id.provably_equal id k.id with Some Type.Equal -> Some v | None -> None

(* typed lookup: first binding whose key id matches [k] *)
let get (t : t) (k : 'a key) : 'a option =
  let rec go = function
    | [] -> None
    | B (id, v) :: rest -> ( match match_binding id v k with Some _ as r -> r | None -> go rest)
  in
  go t

let mem (t : t) (k : 'a key) : bool = Option.is_some (get t k)

(* get or raise — for keys a paw guarantees upstream (e.g. current_user after auth) *)
let get_exn (t : t) (k : 'a key) : 'a =
  match get t k with Some v -> v | None -> invalid_arg ("Assigns.get_exn: missing " ^ k.name)
