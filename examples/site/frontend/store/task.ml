(* The app's task model — ONE declaration, shared verbatim by the server binary and the JS bundle
   (this is the hand-written form of what [@@fennec.collection] will generate): the record, the
   field handles, and the collection declaration. Everything downstream is typed against it — the
   server handler, the publication window, the component's live read, the optimistic stub — so a
   renamed field is a compile error in every file that touches it, and the validation battery runs
   identically on both sides. *)

type t = { id : string; title : string }

let f_id = Codec.doc_id
let f_title = Codec.(req "title" (non_empty (max_len 200 (trim string))))

let collection : t Def.t =
  Def.v "tasks"
    Codec.(
      seal
        (record (fun id title -> { id; title })
        |> field f_id (fun x -> x.id)
        |> field f_title (fun x -> x.title)))
