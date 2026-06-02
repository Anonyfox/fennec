(* Client-side head reconciler. Reads Iso.Head.sources (reactive) and patches
   document.head to match the resolved tag set, keyed by data-ih.

   Hydration-safe: it SEEDS its key->element map from the SSR'd [data-ih] tags
   already in the head. Because the resolve is isomorphic, the desired set on the
   first run equals what SSR emitted, so every tag is found in the seed and merely
   patched in place (a no-op) — no flicker, no duplicate <title>, no <script> re-run.
   When a signal later changes, the effect re-runs and add/patch/removes diff-style. *)
open Js_of_ocaml
open Iso

let start () =
  let head = Dom_html.document##.head in
  let current : (string, Dom_html.element Js.t) Hashtbl.t = Hashtbl.create 16 in
  (* seed from SSR-emitted managed tags *)
  let existing = head##querySelectorAll (Js.string "[data-ih]") in
  for i = 0 to existing##.length - 1 do
    let el : Dom_html.element Js.t = Js.Unsafe.coerce (Js.Opt.get (existing##item i) (fun () -> assert false)) in
    let k = Js.to_string (Js.Opt.get (el##getAttribute (Js.string "data-ih")) (fun () -> Js.string "")) in
    if k <> "" then Hashtbl.replace current k el
  done;
  let set_attrs el a = List.iter (fun (k, v) -> el##setAttribute (Js.string k) (Js.string v)) a in
  let mk key (t : Head.tag) =
    let el =
      match t with
      | Head.Title s -> let e = Dom_html.document##createElement (Js.string "title") in e##.textContent := Js.some (Js.string s); e
      | Head.Meta a -> let e = Dom_html.document##createElement (Js.string "meta") in set_attrs e a; e
      | Head.Link a -> let e = Dom_html.document##createElement (Js.string "link") in set_attrs e a; e
      | Head.Script (a, b) -> let e = Dom_html.document##createElement (Js.string "script") in set_attrs e a; e##.textContent := Js.some (Js.string b); e
      | Head.Json_ld j -> let e = Dom_html.document##createElement (Js.string "script") in e##setAttribute (Js.string "type") (Js.string "application/ld+json"); e##.textContent := Js.some (Js.string j); e
    in
    el##setAttribute (Js.string "data-ih") (Js.string key);
    el
  in
  let patch el (t : Head.tag) =
    match t with
    | Head.Title s -> el##.textContent := Js.some (Js.string s)
    | Head.Meta a | Head.Link a -> set_attrs el a
    | Head.Script (a, b) -> set_attrs el a; el##.textContent := Js.some (Js.string b)
    | Head.Json_ld j -> el##.textContent := Js.some (Js.string j)
  in
  let eff =
    { run = (fun () ->
        let desired = Head.resolve (get Head.sources) in  (* subscribes to the registry *)
        let keyed = List.map (fun t -> (Head.tag_key t, t)) desired in
        let wanted = List.map fst keyed in
        List.iter (fun (k, t) ->
            match Hashtbl.find_opt current k with
            | Some el -> patch el t
            | None -> let el = mk k t in Dom.appendChild head el; Hashtbl.replace current k el)
          keyed;
        let stale = Hashtbl.fold (fun k el acc -> if List.mem k wanted then acc else (k, el) :: acc) current [] in
        List.iter (fun (k, el) -> (try Dom.removeChild head el with _ -> ()); Hashtbl.remove current k) stale);
      deps = [] }
  in
  run_effect eff
