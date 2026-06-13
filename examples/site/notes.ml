(* A fully server-rendered CRUD resource — no client JavaScript, ONE Codec model. [open Fennec.Fur]
   is the whole presentation vocabulary: page/respond/redirect/flash, Form, and the [resource]
   convention (which auto-installs session+CSRF+method-override, auto-loads the entity or 404s, owns
   the id, and auto-parses+validates the form — re-rendering it with per-field errors on failure).
   The store is a tiny in-memory map; swap in a Pulse collection and the handlers don't change. *)
open Fennec.Fur

let secret = "notes-demo-secret-key-change-me"

(* the model: one declaration → form parse + render + validation (no Mongo tail) *)
type t = {
  id : string;
  title : string; [@trim] [@non_empty] [@max_len 80]
  body : string; [@trim] [@max_len 500]
  priority : int; [@min 0] [@max 5]
  pinned : bool;
}
[@@deriving model]

(* in-memory store (mutex-guarded) — the id is assigned by the resource, not here *)
let store : (string, t) Hashtbl.t = Hashtbl.create 16
let mu = Mutex.create ()
let with_mu f = Mutex.lock mu; Fun.protect ~finally:(fun () -> Mutex.unlock mu) f
let all () = with_mu (fun () -> Hashtbl.fold (fun _ v acc -> v :: acc) store []) |> List.sort (fun a b -> match compare b.pinned a.pinned with 0 -> compare a.id b.id | c -> c)
let find id = with_mu (fun () -> Hashtbl.find_opt store id)
let put n = with_mu (fun () -> Hashtbl.replace store n.id n)
let remove id = with_mu (fun () -> Hashtbl.remove store id)

(* views — dead Fur (HTML, no JS). [page]/[h]/[text]/[attr]/[class_]/[frag] all come from [open Fur]. *)
let flash_node = function None -> frag [] | Some m -> h "p" [ class_ "flash" ] [ text m ]
let link href t = h "a" [ attr "href" href ] [ text t ]
let delete_button c id = Form.view c codec ~action:("/notes/" ^ id) ~override:"DELETE" (fun _ -> [ Form.submit "Delete" ])

let index_view c ?flash notes =
  page ~title:"Notes"
    ([ h "h1" [] [ text "Notes" ]; flash_node flash; h "p" [] [ link "/notes/new" "New note" ] ]
    @ [ h "ul" [ class_ "notes" ]
          (List.map
             (fun n -> h "li" [] [ link ("/notes/" ^ n.id) n.title; text (if n.pinned then " 📌" else ""); text (Printf.sprintf " (p%d)" n.priority); delete_button c n.id ])
             notes) ])

let show_view c ?flash n =
  page ~title:n.title
    [ flash_node flash;
      h "h1" [] [ text n.title ];
      h "p" [] [ text n.body ];
      h "p" [] [ text (Printf.sprintf "priority %d%s" n.priority (if n.pinned then " · pinned" else "")) ];
      link ("/notes/" ^ n.id ^ "/edit") "Edit";
      delete_button c n.id;
      link "/notes" "Back" ]

(* the form view: new vs edit differ only in action/method; everything else (value repopulation,
   per-field errors, input type, HTML5 constraints, CSRF) is filled in by [Form.view]/[bound]. *)
let form_view c note =
  let action, override = match note with None -> ("/notes", None) | Some n -> ("/notes/" ^ n.id, Some "PUT") in
  page ~title:(if note = None then "New note" else "Edit note")
    [ Form.view c codec ~action ?override ?entity:note (fun f ->
          [ Form.bound f Fields.title ~label:"Title";
            Form.bound f Fields.body ~label:"Body" ~kind:`Textarea;
            Form.bound f Fields.priority ~label:"Priority (0–5)";
            Form.bound f Fields.pinned ~label:"Pinned";
            Form.submit "Save" ]) ]

(* the resource: one call wires the 7 routes + security paws; handlers get the loaded entity / the
   validated model and answer via respond/redirect *)
let mount ep =
  resource codec "/notes" ~secret ~load:find ~form:form_view
    ~index:(fun c -> let c, fl = flash c in respond c (index_view c ?flash:fl (all ())))
    ~show:(fun c n -> let c, fl = flash c in respond c (show_view c ?flash:fl n))
    ~create:(fun c n -> put n; redirect c ("/notes/" ^ n.id) ~flash:"Note created.")
    ~update:(fun c n -> put n; redirect c ("/notes/" ^ n.id) ~flash:"Note updated.")
    ~destroy:(fun c n -> remove n.id; redirect c "/notes" ~flash:"Note deleted.")
    ep
