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

let () = exit (Fennec_hunt_unit.run ())
