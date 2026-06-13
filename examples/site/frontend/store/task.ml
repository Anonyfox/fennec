(* The app's task model — ONE declaration, shared verbatim by the server binary and the JS bundle
   (the hand-written form of what [@@fennec.collection] generates: a [Fields] module of typed
   handles + the codec + the Def). Everything downstream is typed against it — a renamed field is a
   compile error in every file that touches it, the validation battery runs identically on both
   sides, and [%fields …] projections resolve their handles from [Fields]. [body] is
   required-with-default so a title-only insert stays valid (evolution tolerance). *)

type t = { id : string; title : string; body : string }

module Fields = struct
  let id = Codec.doc_id
  let title = Codec.(req "title" (non_empty (max_len 200 (trim string))))
  let body = Codec.(dft "body" (trim string) "")
end

let collection : t Def.t =
  Def.v "tasks"
    Codec.(
      seal
        (record (fun id title body -> { id; title; body })
        |> field Fields.id (fun x -> x.id)
        |> field Fields.title (fun x -> x.title)
        |> field Fields.body (fun x -> x.body)))
