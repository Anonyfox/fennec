(* Proves sift.mongo's DX is STANDALONE: ONLY fennec.pulse.sift + fennec.pulse.sift.mongo + the
   sift.mongo.ppx driver — NO collection, NO fur. A [@@deriving sift] model, then the [%q] query DSL
   over its [Fields] → a Mongo selector. *)

type t = { status : string; age : int } [@@deriving sift]

let () =
  let where = Filter.all [%q status = "doing" && age > 18] in
  let ok =
    (* the combined query is a Bson selector naming the right fields *)
    List.mem_assoc "status" where
    && List.mem_assoc "age" where
    (* the generated Fields handle carries the wire name *)
    && String.equal (Sift.field_name Fields.status) "status"
  in
  if ok then print_endline "sift.mongo: standalone [@@deriving sift] + [%q] OK"
  else (
    prerr_endline "sift.mongo DSL: FAILED";
    exit 1)
