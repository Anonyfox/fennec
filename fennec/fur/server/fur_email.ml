(* Email CSS inlining — the pure-OCaml RUNTIME apply (see fur_email.mli). The CSS was parsed/scoped at
   build time by [style_extract]; here we only look declarations up by (scope, key) and merge them into
   each element's style attr via [Fur.to_html_styled]. No CSS parser, no Rust. *)

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

(* replace [var(<name>)] and [var(<name>, fallback)] with [value] — [name] carries its leading [--], so
   email clients (which can't resolve custom properties) get a literal *)
let subst_var name value s =
  let needle = "var(" ^ name in
  let nlen = String.length needle and n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if
      !i + nlen <= n
      && String.sub s !i nlen = needle
      && (!i + nlen = n || match s.[!i + nlen] with ')' | ',' | ' ' -> true | _ -> false)
    then
      match String.index_from_opt s (!i + nlen) ')' with
      | Some close ->
        Buffer.add_string buf value;
        i := close + 1
      | None ->
        Buffer.add_char buf s.[!i];
        incr i
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let resolve tokens s = List.fold_left (fun s (name, value) -> subst_var name value s) s tokens

(* the per-element hook handed to [Fur.to_html_styled]: an element styled by the stylesheet is one whose
   [data-fur] scope + class/tag has a rule. Tag first then classes, so class specificity wins downstream. *)
let style_of (t : t) tokens ~tag ~attrs =
  match List.assoc_opt "data-fur" attrs with
  | None -> None (* unscoped: nothing in a scoped stylesheet can target it *)
  | Some scope ->
    let classes =
      match List.assoc_opt "class" attrs with
      | Some c -> String.split_on_char ' ' c |> List.filter (fun s -> s <> "")
      | None -> []
    in
    let decls = List.filter_map (fun key -> Hashtbl.find_opt t (scope, key)) (tag :: classes) in
    ( match decls with
    | [] -> None
    | ds -> Some (resolve tokens (String.concat ";" ds)) )

let to_email t ?(tokens = []) v = Fur.to_html_styled ~style:(style_of t tokens) v
