(* The collection DECLARATION — the pure, instance-free value shared by server and browser (what
   [@@fennec.collection] generates): name + shape + indexes. Server code ATTACHES it to a reactive
   instance at boot (R.Typed.attach); the browser binds it to the live client. One declaration,
   every derivation. *)

type 'a t = { name : string; codec : 'a Sift.t; indexes : Index.t list }

(* every declaration made so far, in declaration order — what the server opens at boot (see [declared]) *)
type any = Any : 'a t -> any

let _declared : any list ref = ref []

let v ?(indexes = []) name codec =
  let d = { name; codec; indexes } in
  _declared := Any d :: !_declared;
  d

let declared () = List.rev !_declared
let name d = d.name
let codec d = d.codec
let indexes d = d.indexes
let validator d = Json_schema.validator d.codec

(* declare this collection's indexes (co-located with the model); registered for boot reconcile *)
let index d ixs = Index.register d.name ixs

(* all indexes for this collection: those on the Def plus any registered via [index] *)
let all_indexes d = d.indexes @ Index.for_collection d.name
