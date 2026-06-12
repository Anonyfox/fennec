(* The collection DECLARATION — the pure, instance-free value shared by server and browser (what
   [@@fennec.collection] generates): name + shape + indexes. Server code ATTACHES it to a reactive
   instance at boot (R.Typed.attach); the browser binds it to the live client. One declaration,
   every derivation. *)

type 'a t = { name : string; codec : 'a Codec.t; indexes : Index.t list }

let v ?(indexes = []) name codec = { name; codec; indexes }
let name d = d.name
let codec d = d.codec
let indexes d = d.indexes
let validator d = Schema.validator d.codec
