(* The collection write/read seam — the isomorphic backend behind a model's generated [create] / [save]
   / [delete] / [find_one] / [where] / [all] / [count] (the Meteor-style methods [@@deriving collection]
   puts on the module). It is the exact sibling of the reactive-read seam [Ddp_client]: an installable
   ref the SERVER fills with the real backend at boot ([Fennec_pulse_app] does it), while the client side
   is a stub. A client writes through a DDP method, never a collection directly, so these are never
   called client-side — and are dead-code-eliminated there when they aren't. Isomorphic: this module is
   plain OCaml over [Def]/[Filter], links into both the native binary and the JS bundle, and holds its
   own ref per build. *)

type backend = {
  create : 'a. 'a Def.t -> 'a -> 'a;
  save : 'a. 'a Def.t -> 'a -> 'a;
  delete : 'a. 'a Def.t -> 'a -> unit;
  find_one : 'a. 'a Def.t -> Filter.t list -> 'a option;
  where : 'a. 'a Def.t -> Filter.t list -> 'a list;
  all : 'a. 'a Def.t -> 'a list;
  count : 'a. 'a Def.t -> Filter.t list -> int;
}

let _backend : backend option ref = ref None
let install (b : backend) = _backend := Some b

let backend () =
  match !_backend with
  | Some b -> b
  | None ->
      failwith
        "Fennec: a collection write/read ran with no backend installed — these are server-side (a \
         client changes data through a method, never the collection directly)"

let create def v = (backend ()).create def v
let save def v = (backend ()).save def v
let delete def v = (backend ()).delete def v
let find_one def sel = (backend ()).find_one def sel
let where def sel = (backend ()).where def sel
let all def = (backend ()).all def
let count def sel = (backend ()).count def sel
