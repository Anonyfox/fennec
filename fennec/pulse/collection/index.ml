(* Declarative indexes over field handles — declared once with the model, reconciled at boot. The
   NAME deterministically encodes the spec (fields+directions+unique) under the [fx_] prefix, so
   reconcile matches by name: a changed declaration yields a new name (old dropped, new created), and
   only [fx_]-named indexes are ever auto-dropped (never _id_ or a hand-made index). *)

type key = string * [ `Asc | `Desc ]
type t = { keys : key list; unique : bool }

let asc f = { keys = [ (Codec.field_name f, `Asc) ]; unique = false }
let desc f = { keys = [ (Codec.field_name f, `Desc) ]; unique = false }
let compound keys = { keys = List.concat_map (fun i -> i.keys) keys; unique = false }
let unique i = { i with unique = true }

let keys_bson i =
  Bson.Document (List.map (fun (n, dir) -> (n, Bson.int (match dir with `Asc -> 1 | `Desc -> -1))) i.keys)
let is_unique i = i.unique

(* the deterministic fennec name: fx_<u|m>_<field_dir__field_dir…> — stable + spec-encoding, so
   reconcile is name-based and a spec change is a new name *)
let name i =
  let parts = List.map (fun (n, d) -> n ^ (match d with `Asc -> "_1" | `Desc -> "_-1")) i.keys in
  Printf.sprintf "fx_%s_%s" (if i.unique then "u" else "m") (String.concat "__" parts)

let is_fennec_name s = String.length s >= 3 && String.sub s 0 3 = "fx_"

(* the declaration registry — like the publication/method registries: app-static, global. Keyed by
   collection NAME (no dependency on Def — Def already holds Index.t). Reconcile reads it at boot. *)
let _registry : (string, t list) Hashtbl.t = Hashtbl.create 16
let register name (ixs : t list) = Hashtbl.replace _registry name ixs
let for_collection name = match Hashtbl.find_opt _registry name with Some l -> l | None -> []
