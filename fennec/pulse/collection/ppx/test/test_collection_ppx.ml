(* GOLDEN: the derived form behaves byte-for-byte like the hand-written builder form — the ppx is
   only the pen. Conventions proven: id → "_id" (ObjectId-aware), trailing underscore stripped for
   the wire key (done_ → "done"), list tolerates absence, [@key]/[@check] deviations honored. *)

module B = Bson

(* the DERIVED side — the official surface *)
type t = {
  id : string;
  title : string; [@check fun s -> String.length s >= 3]
  done_ : bool;
  tags : string list;
  note : string; [@key "remark"]
}
[@@deriving fennec_collection ~name:"gtasks"]

(* the HAND-WRITTEN twin (what MODEL.md documents as the expansion) *)
let twin =
  Codec.(
    seal
      (record (fun id title done_ tags note -> { id; title; done_; tags; note })
      |> field doc_id (fun x -> x.id)
      |> field (req "title" (check (fun s -> String.length s >= 3) string)) (fun x -> x.title)
      |> field (req "done" bool) (fun x -> x.done_)
      |> field (opt_list "tags" string) (fun x -> x.tags)
      |> field (req "remark" string) (fun x -> x.note)))

let sample = { id = "a1"; title = "Hello"; done_ = true; tags = [ "x" ]; note = "n" }

let%test "golden: derived enc/dec ≡ hand-written (round-trip + identical wire bytes)" =
  let d1 = codec.Codec.enc sample and d2 = twin.Codec.enc sample in
  B.equal d1 d2
  && (match Codec.decode codec d2 with Ok v -> v = sample | Error _ -> false)
  && (match Codec.decode twin d1 with Ok v -> v = sample | Error _ -> false)

let%test "golden: conventions — done_ keys as \"done\"; [@key] honored; id is _id; absent list = []" =
  (match codec.Codec.enc sample with
  | B.Document kvs ->
      List.mem_assoc "done" kvs && List.mem_assoc "remark" kvs && List.mem_assoc "_id" kvs
      && not (List.mem_assoc "done_" kvs) && not (List.mem_assoc "note" kvs)
  | _ -> false)
  &&
  match Codec.decode codec (B.doc [ ("_id", B.str "z"); ("title", B.str "abc"); ("done", B.Bool false); ("remark", B.str "r") ]) with
  | Ok v -> v.tags = []
  | Error _ -> false

let%test "golden: [@check] enforces (same battery as the server); view reflection identical" =
  (match Codec.decode codec (B.doc [ ("_id", B.str "z"); ("title", B.str "ab"); ("done", B.Bool false); ("remark", B.str "r") ]) with
  | Error _ -> true
  | Ok _ -> false)
  && Codec.view codec = Codec.view twin
  && Def.name collection = "gtasks"
  && B.equal (Def.validator collection) (Schema.validator twin)

(* ── projections: [%fields …] — Meteor's { fields: {…} } made a type-safe object ── *)
let%test "projection: [%fields] yields the wire doc AND an object of exactly those fields" =
  let card = [%fields title; done_] in
  (* the Mongo projection document — Meteor's { title: 1, done: 1 } *)
  (match Proj.project_doc card with
  | B.Document [ ("_id", B.Int 0); ("title", B.Int 1); ("done", B.Int 1) ] -> true (* _id auto-trimmed *)
  | _ -> false)
  &&
  (* decode a full stored doc into the PROJECTED object — only title/done survive, typed *)
  let stored = B.doc [ ("_id", B.str "z"); ("title", B.str "Hello"); ("done", B.Bool true); ("remark", B.str "x"); ("tags", B.array []) ] in
  match Proj.decode card stored with
  | Ok o -> o#title = "Hello" && o#done_ = true (* o#remark would be a COMPILE error: no method *)
  | Error _ -> false

let%test "projection: id is shipped only when explicitly projected (else _id:0 trims the wire)" =
  (match Proj.project_doc [%fields id; title] with
  | B.Document [ ("_id", B.Int 1); ("title", B.Int 1) ] -> true (* asked for id → no _id:0, _id:1 *)
  | _ -> false)
  &&
  let o = match Proj.decode [%fields id; title] (B.doc [ ("_id", B.str "k"); ("title", B.str "Title") ]) with Ok o -> o | Error _ -> assert false in
  o#id = "k" && o#title = "Title"

let%test "projection: $slice on an array field — wire carries {$slice}, the list type is unchanged" =
  (match Proj.project_doc [%fields title; slice tags 3] with
  | B.Document [ ("_id", B.Int 0); ("title", B.Int 1); ("tags", B.Document [ ("$slice", B.Int 3) ]) ] -> true
  | _ -> false)
  && (match Proj.project_doc [%fields slice tags 2 5] with
     | B.Document [ ("_id", B.Int 0); ("tags", B.Document [ ("$slice", B.Array [ B.Int 2; B.Int 5 ]) ]) ] -> true
     | _ -> false)
  &&
  (* the object still decodes tags as a string list (the slice trimmed the array, not the type) *)
  let o = match Proj.decode [%fields slice tags 2] (B.doc [ ("tags", B.array [ B.str "a"; B.str "b" ]) ]) with
    | Ok o -> o | Error _ -> assert false in
  o#tags = [ "a"; "b" ]

let%test "projection: a missing projected field surfaces as a decode error (skip-policy fodder)" =
  let card = [%fields title; done_] in
  match Proj.decode card (B.doc [ ("title", B.str "only") ]) with Error _ -> true | Ok _ -> false

(* ── embedded records + dotted-path projections (nested object) ── *)
module Author = struct
  type t = { name : string; email : string } [@@deriving fennec_collection ~name:"_authors"]
end

module Post = struct
  type t = { id : string; title : string; author : Author.t }
  [@@deriving fennec_collection ~name:"posts"]
end

let%test "embedded record: the deriver nests the codec; the doc round-trips through Author.codec" =
  let p = { Post.id = "p1"; title = "Hi"; author = { Author.name = "Ada"; email = "a@x.io" } } in
  match Codec.decode Post.codec (Post.codec.Codec.enc p) with
  | Ok q -> q = p && (match Post.codec.Codec.enc p with
                      | B.Document kvs -> (match List.assoc_opt "author" kvs with Some (B.Document _) -> true | _ -> false)
                      | _ -> false)
  | Error _ -> false

let%test "dotted projection: author/name yields wire author.name AND a nested object" =
  let proj = Post.([%fields title; author / name]) in
  (match Proj.project_doc proj with
  | B.Document [ ("_id", B.Int 0); ("title", B.Int 1); ("author.name", B.Int 1) ] -> true
  | _ -> false)
  &&
  (* decode a full stored post → the projected nested object; o#author#name typed, o#author#email a COMPILE error *)
  let stored = B.doc [ ("_id", B.str "p1"); ("title", B.str "Hi");
                       ("author", B.doc [ ("name", B.str "Ada"); ("email", B.str "a@x.io") ]) ] in
  match Proj.decode proj stored with
  | Ok o -> o#title = "Hi" && o#author#name = "Ada"
  | Error _ -> false

let%test "dotted projection: siblings under one head merge into one nested object" =
  let proj = Post.([%fields author / name; author / email]) in
  (match Proj.project_doc proj with
  | B.Document [ ("_id", B.Int 0); ("author.name", B.Int 1); ("author.email", B.Int 1) ] -> true
  | _ -> false)
  &&
  let stored = B.doc [ ("author", B.doc [ ("name", B.str "Ada"); ("email", B.str "a@x.io") ]) ] in
  match Proj.decode proj stored with Ok o -> o#author#name = "Ada" && o#author#email = "a@x.io" | Error _ -> false

let () = exit (Fennec_hunt_unit.run ())
