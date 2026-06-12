(* The typed-collection vocabulary, phase by phase. Here: Schema — $jsonSchema golden assertions
   (hints translate; optionality = required-omission; variants = oneOf; evolution tolerance). *)

module B = Bson

let get d k = match d with B.Document kvs -> List.assoc_opt k kvs | _ -> None
let getd d k = match get d k with Some (B.Document _ as x) -> x | _ -> B.Null

type t = { id : string; title : string; tags : string list; note : string option }

let codec =
  Codec.(
    seal
      (record (fun id title tags note -> { id; title; tags; note })
      |> field doc_id (fun x -> x.id)
      |> field (req "title" (min_len 3 (max_len 9 string))) (fun x -> x.title)
      |> field (opt_list "tags" (slug string)) (fun x -> x.tags)
      |> field (opt "note" string) (fun x -> x.note)))

let%test "schema: object with properties; required omits opt/opt_list; stacked hints merge" =
  let s = Schema.json_schema (Codec.view codec) in
  (match get s "required" with
  | Some (B.Array req) ->
      let names = List.filter_map (function B.String x -> Some x | _ -> None) req in
      List.sort compare names = [ "_id"; "title" ]
  | _ -> false)
  && (let title = getd (getd s "properties") "title" in
      get title "minLength" = Some (B.Int 3)
      && get title "maxLength" = Some (B.Int 9)
      && get title "bsonType" = Some (B.String "string"))
  && (let tags = getd (getd s "properties") "tags" in
      get tags "bsonType" = Some (B.String "array")
      && match get (getd tags "items") "pattern" with Some (B.String _) -> true | _ -> false)
  && (let idf = getd (getd s "properties") "_id" in
      match get idf "bsonType" with Some (B.Array _) -> true | _ -> false)
  && get s "additionalProperties" = None (* evolution tolerance: legacy fields stay writable *)

type p = Fixed of float | Free

let p_codec =
  Codec.(
    variant ~tag:"kind"
      [ case "fixed" (record (fun a -> Fixed a) |> field (req "amount" (positive float)) (function Fixed a -> a | _ -> 0.))
          ~inj:Fun.id ~proj:(function Fixed _ as v -> Some v | _ -> None);
        case "free" (record Free) ~inj:Fun.id ~proj:(function Free -> Some Free | _ -> None) ])

let%test "schema: variants render as oneOf with the tag pinned per case; numeric hints translate" =
  match get (Schema.json_schema (Codec.view p_codec)) "oneOf" with
  | Some (B.Array [ fixed; free ]) ->
      (let tagf = getd (getd fixed "properties") "kind" in
       get tagf "enum" = Some (B.Array [ B.str "fixed" ]))
      && (let amount = getd (getd fixed "properties") "amount" in
          match get amount "minimum" with Some (B.Float _) -> true | _ -> false)
      && (match get (getd free "properties") "kind" with Some _ -> true | None -> false)
  | _ -> false

let%test "schema: the validator wraps as { $jsonSchema: … }; maps render additionalProperties" =
  (match Schema.validator codec with
  | B.Document [ ("$jsonSchema", B.Document _) ] -> true
  | _ -> false)
  &&
  let mc = Codec.(seal (record (fun m -> m) |> field (req "i18n" (str_map (non_empty string))) (fun m -> m))) in
  let s = Schema.json_schema (Codec.view mc) in
  let i18n = getd (getd s "properties") "i18n" in
  get i18n "bsonType" = Some (B.String "object")
  && (match get (getd i18n "additionalProperties") "minLength" with Some (B.Int 1) -> true | _ -> false)

let () = exit (Fennec_hunt_unit.run ())
