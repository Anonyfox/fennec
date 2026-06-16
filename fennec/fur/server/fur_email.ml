(* Email CSS inlining — the pure-OCaml RUNTIME apply (see fur_email.mli). The CSS was parsed, scoped AND
   var()-resolved at BUILD time by [style_extract]; here we only look declarations up by (scope, key) and
   merge them into each element's style attr via [Fur.to_html_styled]. No CSS parser, no Rust, no tokens. *)

type t = (string * string, string) Hashtbl.t (* (scope, selector-key) -> declarations *)

let of_rules rules =
  let h : t = Hashtbl.create 64 in
  List.iter
    (fun (scope, key, decls) ->
      (* same key seen twice (two rules) → concatenate, last wins in the style attr (the cascade) *)
      let prev = match Hashtbl.find_opt h (scope, key) with Some p -> p ^ ";" | None -> "" in
      Hashtbl.replace h (scope, key) (prev ^ decls))
    rules;
  h

(* The ambient stylesheet. The generated [Site_styles] calls [install] at link time, so [to_email] works
   with no userland wiring — exactly as a page renders without the app touching its [%%style]. *)
let _ambient : t option ref = ref None
let install rules = _ambient := Some (of_rules rules)

(* the per-element hook handed to [Fur.to_html_styled]: an element is styled by the stylesheet when its
   [data-fur] scope + a class/tag has a rule. Tag first then classes, so class specificity wins. *)
let style_of (t : t) ~tag ~attrs =
  match List.assoc_opt "data-fur" attrs with
  | None -> None (* unscoped: nothing in a scoped stylesheet can target it *)
  | Some scope ->
    let classes =
      match List.assoc_opt "class" attrs with
      | Some c -> String.split_on_char ' ' c |> List.filter (fun s -> s <> "")
      | None -> []
    in
    ( match List.filter_map (fun key -> Hashtbl.find_opt t (scope, key)) (tag :: classes) with
    | [] -> None
    | ds -> Some (String.concat ";" ds) )

let to_email ?stylesheet v =
  match (match stylesheet with Some _ as s -> s | None -> !_ambient) with
  | None -> Fur.to_html v (* no stylesheet installed → plain HTML, no inlining *)
  | Some t -> Fur.to_html_styled ~style:(style_of t) v
