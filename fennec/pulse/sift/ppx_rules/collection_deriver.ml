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
      (* the verbs, directly on the model module — no functor, no handle threaded, the collection IS the
         object (Meteor's Tasks.find / Tasks.insert). Two families:
         - the reactive READS [find]/[project] — a cursor over the live cache (browser) / SSR-seeded
           (server), via the isomorphic [Ddp_client] ambient connection;
         - the server WRITES/READS [create]/[save]/[delete]/[find_one]/[where]/[all]/[count] — over the
           isomorphic [Coll_writer] seam the server fills at boot. A CLIENT still changes data through a
           method (these stub + DCE there); a server method handler OR a [@workflow] calls them directly. *)
      let find = [%stri let find ?where ?sort ?skip ?limit () =
        Ddp_client.find_c (Ddp_client.default ()) collection ?where ?sort ?skip ?limit ()] in
      let project = [%stri let project p ?where ?sort ?skip ?limit () =
        Ddp_client.find_p (Ddp_client.default ()) collection p ?where ?sort ?skip ?limit ()] in
      let create = [%stri let create v = Coll_writer.create collection v] in
      let save = [%stri let save v = Coll_writer.save collection v] in
      let delete = [%stri let delete v = Coll_writer.delete collection v] in
      let find_one = [%stri let find_one sel = Coll_writer.find_one collection sel] in
      let where = [%stri let where sel = Coll_writer.where collection sel] in
      let all = [%stri let all () = Coll_writer.all collection] in
      let count = [%stri let count sel = Coll_writer.count collection sel] in
      core @ [ coll; find; project; create; save; delete; find_one; where; all; count ]
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
