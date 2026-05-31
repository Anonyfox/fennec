(* Client (CSR) runtime for <Head>, compiled by Melange. Mirrors the native SSR
   runtime's [make] surface so the SAME shared <Head> compiles to both targets.

   Model: each <Head> is a component that owns a layout-effect. On mount (and
   whenever its tags change) it writes its tags into a module-level registry under
   a stable instance id and re-applies the merged head to the live document; on
   unmount it removes its entry and re-applies. This is driven by the PAGE's
   render — so client-side navigation (which re-renders the page and mounts/
   unmounts its <Head>) updates the document head correctly, without needing a
   re-rendering provider (a provider's effect would only fire once in an SPA).

   The merge is the SAME pure Fennec_head.Head.merge the server uses, so after
   hydration the client computes the identical head — no flash. Registry order =
   mount order ≈ tree order (parent before child), so merge's last-wins gives
   inside-out precedence. We tag our managed nodes with data-fennec-head so we
   touch only our own, leaving the template's static <head> alone.

   useLayoutEffect1 keyed on a serialization of the tags re-runs on content
   change; useLayoutEffect runs synchronously before paint, so the title never
   flickers. *)

module Head = Fennec_head.Head

(* registry: instance id -> its tags. A monotonic counter assigns ids in mount
   order, and we keep that order for the merge. *)
let registry : (int * Head.tag list) list ref = ref []
let next_id = ref 0

let set_entry id tags =
  registry := List.filter (fun (i, _) -> i <> id) !registry @ [ (id, tags) ]

let remove_entry id = registry := List.filter (fun (i, _) -> i <> id) !registry

let all_tags () = List.concat_map snd !registry

(* ---- DOM application (raw JS, browser only) ---- *)

let set_title : string -> unit = [%mel.raw {| function (t) { document.title = t; } |}]

let apply_head_html : string -> unit =
  [%mel.raw
    {| function (html) {
         var head = document.head;
         var old = head.querySelectorAll('[data-fennec-head]');
         old.forEach(function (n) { n.parentNode.removeChild(n); });
         if (!html) return;
         var tmp = document.createElement('head');
         tmp.innerHTML = html;
         Array.prototype.slice.call(tmp.childNodes).forEach(function (n) {
           if (n.nodeType === 1) {
             if (n.tagName === 'TITLE') return; /* title via document.title */
             n.setAttribute('data-fennec-head', '');
             head.appendChild(n);
           }
         });
       } |}]

(* recompute the merged head from all registry entries and apply it *)
let reapply () =
  let tags = Head.merge (all_tags ()) in
  (match Head.title_of tags with Some t -> set_title t | None -> ());
  apply_head_html (Head.to_html tags)

(* a stable key for the effect dependency: the merged-head HTML of THIS instance's
   tags changes iff its content changes *)
let tags_key (tags : Head.tag list) : string = Head.to_html tags

(* the internal hook-bearing component: registers this instance's tags and
   applies the merged head, reverting on unmount/content change *)
let[@react.component] component ~(tags : Head.tag list) =
  let id_ref = React.useRef (-1) in
  if id_ref.current < 0 then (
    id_ref.current <- !next_id;
    incr next_id);
  let id = id_ref.current in
  React.useLayoutEffect1
    (fun () ->
      set_entry id tags;
      reapply ();
      Some
        (fun () ->
          remove_entry id;
          reapply ()))
    [| tags_key tags |];
  React.null

(* The PUBLIC <Head>. A PLAIN labelled function (identical signature to the native
   runtime's [make]) so the one shared component compiles to both targets. It
   delegates to the internal hook component. *)
let make ?title ?description ?canonical ?(extra = []) () : React.element =
  let tags = Head.of_props ?title ?description ?canonical ~extra () in
  component ~tags
