(* serde.ml — the Format abstraction (the module is [Serde], not [Format], to avoid shadowing the
   stdlib [Format] used for pretty-printing). A READER (Deserializer, PULL) destructures an arbitrary
   source value into the shape-leaf semantics; a WRITER (Serializer, PUSH) builds an arbitrary sink
   from them. ONE shape-directed interpreter ({!read}/{!write}) drives ANY format — the neutral
   {!Value} tree, relaxed JSON, later XML/CSV — so a new format implements ~a dozen leaf hooks rather
   than re-walking the GADT. RAW: refinement checks run in the shared {!Engine.run_checks} phase, not
   here, so a format never sees a refinement.

   What stays SPECIALIZED (not driven through here): the zero-copy BSON byte paths
   ({!Bson_reader}/{!Bson_writer}, the hot storage/wire path) and the reference BSON-tree engine
   ({!Engine.read}/{!Engine.write} — the decode oracle, and the only path that round-trips the
   lossless {!Bson.t} escape). This abstraction serves the OTHER formats, where the per-call module
   indirection is off the hot path and the neutral-model lossiness of the escape is inherent anyway. *)

open Shape

let typ_err what got = Error [ mkerr ~code:"type" (Printf.sprintf "expected %s, got %s" what got) ]

(* A Deserializer: classify + extract one source value. [None] from an extractor means "not that
   kind" → the driver raises a path-tagged type error. Containers yield SUB-source positions, so the
   driver recurses without the format pre-materializing a whole tree. The leaf extractors carry each
   format's own coercions (an integral float read as an int, etc.), mirroring the BSON engine. *)
module type READER = sig
  type src

  val describe : src -> string (* a short kind name, for "expected X, got <describe>" *)
  val is_null : src -> bool
  val to_bool : src -> bool option
  val to_int : src -> int option
  val to_float : src -> float option
  val to_string : src -> string option
  val to_id : src -> string option (* an id leaf — String, or a format's native id (BSON ObjectId) *)
  val to_date : src -> int64 option (* a date leaf — ms since epoch *)
  val to_dyn : src -> Value.t (* the [TDyn] escape — read any source value as a neutral Value *)
  val to_list : src -> src list option
  val to_assoc : src -> (string * src) list option

  (* [from_string]: re-read a submitted String as a source leaf of the inner shape (forms / query
     strings / lenient JSON deliver numbers and bools stringly). *)
  val coerce_leaf : 'a. 'a shape -> string -> src
end

(* A Serializer: build one sink value from a shape-leaf. Tree formats ([Value], a BSON tree) return
   the built node; the hot streaming encoders (JSON, BSON bytes) stay specialized rather than route
   through an [out]-building interface. *)
module type WRITER = sig
  type out

  val null : out
  val bool : bool -> out
  val int : int -> out
  val float : float -> out
  val string : string -> out
  val id : string -> out
  val date : int64 -> out
  val dyn : Value.t -> out (* the [TDyn] escape *)
  val list : out list -> out
  val assoc : (string * out) list -> out
end

(* ---- the generic decode (RAW; the check phase runs separately, exactly as {!Engine.read}) -------- *)

let rec read : type a s. (module READER with type src = s) -> a shape -> s -> (a, error list) result =
 fun ((module R) as m) shape src ->
  let bad what = typ_err what (R.describe src) in
  match shape with
  | TString -> ( match R.to_string src with Some x -> Ok x | None -> bad "string")
  | TInt -> ( match R.to_int src with Some x -> Ok x | None -> bad "int")
  | TFloat { allow_nonfinite } -> (
      match R.to_float src with
      | Some f -> if (not allow_nonfinite) && not (Float.is_finite f) then Error [ mkerr ~code:"finite" "non-finite float" ] else Ok f
      | None -> bad "float")
  | TBool -> ( match R.to_bool src with Some x -> Ok x | None -> bad "bool")
  | TDate -> ( match R.to_date src with Some x -> Ok x | None -> bad "date")
  | TId -> ( match R.to_id src with Some x -> Ok x | None -> bad "id (string or objectid)")
  | TDyn -> Ok (R.to_dyn src)
  | TUnit -> if R.is_null src then Ok () else bad "null"
  | TList el -> ( match R.to_list src with Some xs -> read_list m el xs | None -> bad "array")
  | TOption el -> if R.is_null src then Ok None else ( match read m el src with Ok x -> Ok (Some x) | Error e -> Error e)
  | TMap el -> ( match R.to_assoc src with Some kvs -> read_map m el kvs | None -> bad "document")
  | TCheck (_, _, _, inner) -> read m inner src (* RAW: refinements run in run_checks *)
  | TNorm (f, inner) -> ( match read m inner src with Ok v -> Ok (f v) | Error e -> Error e)
  | TConv (_inj, proj, inner) -> ( match read m inner src with Ok v -> ( match proj v with Ok x -> Ok x | Error msg -> Error [ mkerr msg ]) | Error e -> Error e)
  | TObj o -> ( match R.to_assoc src with Some kvs -> o.decode_src (field_reader_of m kvs) | None -> bad "document")
  | TVariant { tag; cases } -> read_variant m tag cases src
  | TLazy l -> read m (Lazy.force l) src
  | TCoerce inner -> ( match R.to_string src with Some s -> read m inner (R.coerce_leaf inner s) | None -> read m inner src)

and read_list : type a s. (module READER with type src = s) -> a shape -> s list -> (a list, error list) result =
 fun m el xs ->
  let oks, errs, _ =
    List.fold_left
      (fun (oks, errs, i) x ->
        match read m el x with
        | Ok v -> (v :: oks, errs, i + 1)
        | Error es -> (oks, List.rev_append (List.map (at (string_of_int i)) es) errs, i + 1))
      ([], [], 0) xs
  in
  if errs = [] then Ok (List.rev oks) else Error (List.rev errs)

and read_map : type a s. (module READER with type src = s) -> a shape -> (string * s) list -> ((string * a) list, error list) result =
 fun m el kvs ->
  let oks, errs =
    List.fold_left
      (fun (oks, errs) (k, v) -> match read m el v with Ok x -> ((k, x) :: oks, errs) | Error es -> (oks, List.rev_append (List.map (at k) es) errs))
      ([], []) kvs
  in
  if errs = [] then Ok (List.rev oks) else Error (List.rev errs)

(* a {!Shape.field_reader} over a source's assoc — the SAME constructor threading the tree/buffer
   paths use, so records/variants decode identically regardless of format. First key wins (assoc
   semantics), matching the engine's index. *)
and field_reader_of : type s. (module READER with type src = s) -> (string * s) list -> field_reader =
 fun m kvs ->
  {
    read_field =
      (fun f ->
        match List.assoc_opt f.key kvs with
        | Some sub -> ( match read m f.item sub with Ok x -> Ok x | Error es -> Error (List.map (at f.key) es))
        | None -> ( match f.fallback with Some d -> Ok d | None -> Error [ mkerr ~code:"required" ~path:[ f.key ] "is required" ]));
  }

and read_variant : type r s. (module READER with type src = s) -> string -> r case list -> s -> (r, error list) result =
 fun ((module R) as m) tag cases src ->
  match R.to_assoc src with
  | None -> typ_err "document" (R.describe src)
  | Some kvs -> (
      match List.assoc_opt tag kvs with
      | None -> Error [ mkerr (Printf.sprintf "missing tag field %s" tag) ]
      | Some tagsrc -> (
          match R.to_string tagsrc with
          | None -> Error [ mkerr (Printf.sprintf "missing tag field %s" tag) ]
          | Some k -> (
              match List.find_opt (fun (Case c) -> c.name = k) cases with
              | Some (Case c) -> ( match c.body.decode_src (field_reader_of m kvs) with Ok a -> Ok (c.inject a) | Error e -> Error (List.map (at k) e))
              | None -> Error [ mkerr (Printf.sprintf "unknown %s %S" tag k) ])))

(* ---- the generic encode (TOTAL, mirrors {!Engine.write}; omission mirrors {!encode_field}) ------- *)

let rec write : type a o. (module WRITER with type out = o) -> a shape -> a -> o =
 fun ((module W) as m) shape v ->
  match shape with
  | TString -> W.string v
  | TInt -> W.int v
  | TFloat _ -> W.float v
  | TBool -> W.bool v
  | TDate -> W.date v
  | TId -> W.id v
  | TDyn -> W.dyn v
  | TUnit -> W.null
  | TList el -> W.list (List.map (write m el) v)
  | TOption el -> ( match v with Some x -> write m el x | None -> W.null)
  | TMap el -> W.assoc (List.map (fun (k, x) -> (k, write m el x)) v)
  | TCheck (_, _, _, inner) -> write m inner v
  | TNorm (f, inner) -> write m inner (f v)
  | TConv (inj, _, inner) -> write m inner (inj v)
  | TObj o -> W.assoc (write_members m o.members v)
  | TVariant { tag; cases } -> write_variant m tag cases v
  | TLazy l -> write m (Lazy.force l) v
  | TCoerce inner -> write m inner v

and write_members : type r o. (module WRITER with type out = o) -> r bound_field list -> r -> (string * o) list =
 fun m members v -> List.filter_map (fun (Bound_field f) -> let x = f.get v in if f.omit x then None else Some (f.name, write m f.shape x)) members

and write_variant : type r o. (module WRITER with type out = o) -> string -> r case list -> r -> o =
 fun ((module W) as m) tag cases v ->
  let rec go = function
    | [] -> invalid_arg "Sift: variant value matches no declared case"
    | Case c :: rest -> ( match c.project v with Some a -> W.assoc ((tag, W.string c.name) :: write_members m c.body.members a) | None -> go rest)
  in
  go cases
