(* The persistent write outbox (PWA tier 3): the codec for buffered method calls that must survive a
   page reload. An entry is exactly what re-issuing needs — (method name, wire params, seed) —
   because closures don't survive: the resolver is gone (fire-and-forget after a reload, like
   Meteor), and the stub re-runs via Method.stub_replay with the SAME seed, so the restored
   simulation mints identical ids. Pure (bson_json), unit-tested natively; the browser client owns
   when to persist/restore. *)

module BJ = Fennec_mongo_bson_json.Bson_json

type entry = { name : string; params : Bson.t list; seed : string option }

let encode (entries : entry list) : string =
  BJ.to_string
    (Bson.Array
       (List.map
          (fun e ->
            Bson.Document
              (("n", Bson.String e.name) :: ("p", Bson.Array e.params)
              :: (match e.seed with Some s -> [ ("s", Bson.String s) ] | None -> [])))
          entries))

(* malformed / legacy payloads decode to [] (and malformed items are skipped) — never a crash *)
let decode (s : string) : entry list =
  match BJ.of_string_opt s with
  | Some (Bson.Array items) ->
      List.filter_map
        (function
          | Bson.Document kvs -> (
              match (List.assoc_opt "n" kvs, List.assoc_opt "p" kvs) with
              | Some (Bson.String name), Some (Bson.Array params) ->
                  let seed = match List.assoc_opt "s" kvs with Some (Bson.String x) -> Some x | _ -> None in
                  Some { name; params; seed }
              | _ -> None)
          | _ -> None)
        items
  | _ -> []
