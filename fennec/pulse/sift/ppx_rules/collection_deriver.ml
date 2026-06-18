(* The [@@deriving collection] deriver — the reactive-store tail; one of the three modules of
   fennec.pulse.sift.ppx.rules. The codec/Fields generation is {!Sift_ppx_rules}; the Mongo query DSL
   ([%q] [%fields] [%sort] [%set] [%index]) is {!Sift_mongo_ppx_rules}. [@@deriving collection ~name:"t"]
   reuses {!Sift_ppx_rules.model_core} and adds the runtime tail:
     let collection = Def.v "name" codec     (* the collection declaration (sift.mongo) *)
     let find / project = Ddp_client.…       (* the reactive store verbs — fennec runtime *)
   It auto-registers globally on load (forced via the exported [deriver]); fur.ppx and the standalone
   fennec.pulse.sift.ppx fold it into their single ppx pass alongside the model + query-DSL rules. *)

open Ppxlib
open Sift_ppx_rules

let expand ~ctxt (_rec : rec_flag) (tds : type_declaration list) (cname : string) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ { ptype_kind = Ptype_record labels; ptype_name; _ } ] when ptype_name.txt = "t" ->
      let module B = Ast_builder.Default in
      let core = model_core ~loc labels in
      let coll = [%stri let collection = Def.v [%e B.estring ~loc cname] codec] in
      (* the reactive READ verbs, directly on the model module — no functor, no view binding, the
         collection IS the object (Meteor's Tasks.find). Ambient connection (one per page); reads
         only (writes go through methods, by decree). A reactive cursor over the live cache: live in
         the browser, SSR-seeded server-side. A server-handler one-shot read uses the typed handle
         (T.find) — distinguished by type (this returns a Fur signal, not a list). *)
      let find = [%stri let find ?where ?sort ?skip ?limit () =
        Ddp_client.find_c (Ddp_client.default ()) collection ?where ?sort ?skip ?limit ()] in
      let project = [%stri let project p ?where ?sort ?skip ?limit () =
        Ddp_client.find_p (Ddp_client.default ()) collection p ?where ?sort ?skip ?limit ()] in
      core @ [ coll; find; project ]
  | _ ->
      Location.raise_errorf ~loc "fennec.collection: expects a single record type named t"

(* the deriver registers GLOBALLY on module load (forced when fur.ppx / the standalone references
   this module), so [@@deriving fennec_collection] works in whichever single driver links us *)
let deriver =
  Deriving.add "collection"
    ~str_type_decl:
      (Deriving.Generator.V2.make
         Deriving.Args.(empty +> arg "name" (Ast_pattern.estring __))
         (fun ~ctxt (rf, tds) name ->
           match name with
           | Some n -> expand ~ctxt rf tds n
           | None ->
               Location.raise_errorf
                 ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
                 "fennec.collection: the collection name is required — [@@deriving collection ~name:\"tasks\"]"))
