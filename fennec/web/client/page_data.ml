(* The client/isomorphic read of a Page's cross-stage payload: the SAME Codec the server encoded with
   decodes it back. On the server the value comes from the SSR seed; on the client from
   window.__FUR_DATA__ — resolved identically, so the view hydrates byte-for-byte. Declare it INSIDE
   the view (per render) so it reads the current request's seed. *)
module Bson_json = Fennec_mongo_bson_json.Bson_json

let resource (codec : 'a Codec.t) ~key ~fallback : 'a Fur.Data.t =
  Fur.Data.resource ~key ~fallback
    ~decode:(fun s ->
      match Bson_json.of_string_opt s with
      | Some b -> ( match Codec.decode codec b with Ok v -> v | Error _ -> fallback)
      | None -> fallback)
    ()
