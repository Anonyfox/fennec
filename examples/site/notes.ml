(* A fully server-rendered CRUD resource — the typed middle layer (fennec.web) end to end, with NO
   client JavaScript. ONE Codec model drives form parsing, form rendering, JSON output, and
   validation; dead Fur views render the HTML; Resource.crud wires the conventional REST routes;
   Session carries post-redirect-get flash; Csrf + Method_override secure the HTML form writes
   (PUT/DELETE simulated from POST). The store here is a tiny in-memory map — the point is the web
   layer, not persistence (swap in a Pulse collection and the handlers are unchanged). *)

module Conn = Fennec.Conn
module Endpoint = Fennec.Endpoint
module Csrf = Fennec_server.Csrf
module Session = Fennec_server.Session
module Method_override = Fennec_server.Method_override
module Form = Fennec_web.Form
module View = Fennec_web.View
module Action = Fennec_web.Action
module Respond = Fennec_web.Respond
module Resource = Fennec_web.Resource

let secret = "notes-demo-secret-key-change-me"

(* ── the model: one declaration → parse + render + JSON + validation (no Mongo tail) ── *)
type t = {
  id : string;
  title : string; [@trim] [@non_empty] [@max_len 80]
  body : string; [@trim] [@max_len 500]
  priority : int; [@min 0] [@max 5]
  pinned : bool;
}
[@@deriving model]

(* ── in-memory store (mutex-guarded; ids are simple sequential strings for clean URLs) ── *)
let store : (string, t) Hashtbl.t = Hashtbl.create 16
let mu = Mutex.create ()
let seq = ref 0
let with_mu f = Mutex.lock mu; Fun.protect ~finally:(fun () -> Mutex.unlock mu) f
let all () = with_mu (fun () -> Hashtbl.fold (fun _ v acc -> v :: acc) store []) |> List.sort (fun a b -> match compare b.pinned a.pinned with 0 -> compare a.id b.id | c -> c)
let find id = with_mu (fun () -> Hashtbl.find_opt store id)
let put n = with_mu (fun () -> Hashtbl.replace store n.id n)
let remove id = with_mu (fun () -> Hashtbl.remove store id)
let next_id () = with_mu (fun () -> incr seq; string_of_int !seq)

(* ── flash (post-redirect-get feedback via the session) ── *)
let set_flash c msg = Session.set c "flash" msg
let take_flash c = match Session.get c "flash" with Some m -> (Session.delete c "flash", Some m) | None -> (c, None)

(* ── views: dead Fur (rendered to HTML, no JS) ── *)
let flash_node = function None -> Fur.frag [] | Some m -> Fur.h "p" [ Fur.class_ "flash" ] [ Fur.text m ]
let link href text = Fur.h "a" [ Fur.attr "href" href ] [ Fur.text text ]

let delete_form ~conn id =
  Form.form ~action:("/notes/" ^ id) ~override:"DELETE" ~csrf:(Csrf.token ~secret conn) [ Form.submit "Delete" ]

let index_view ~conn ?flash notes =
  View.page ~title:"Notes"
    ([ Fur.h "h1" [] [ Fur.text "Notes" ]; flash_node flash; Fur.h "p" [] [ link "/notes/new" "New note" ] ]
    @ [ Fur.h "ul" [ Fur.class_ "notes" ]
          (List.map
             (fun n ->
               Fur.h "li" []
                 [ link ("/notes/" ^ n.id) n.title;
                   Fur.text (if n.pinned then " 📌" else "");
                   Fur.text (Printf.sprintf " (p%d)" n.priority);
                   delete_form ~conn n.id ])
             notes) ])

let show_view ~conn ?flash n =
  View.page ~title:n.title
    [ flash_node flash;
      Fur.h "h1" [] [ Fur.text n.title ];
      Fur.h "p" [] [ Fur.text n.body ];
      Fur.h "p" [] [ Fur.text (Printf.sprintf "priority %d%s" n.priority (if n.pinned then " · pinned" else "")) ];
      link ("/notes/" ^ n.id ^ "/edit") "Edit";
      delete_form ~conn n.id;
      link "/notes" "Back" ]

(* a field's current value: what was submitted (re-render after error) else the entity else empty *)
let prefill conn note name from_note = match Conn.body_param conn name with Some v -> v | None -> ( match note with Some n -> from_note n | None -> "")

let form_view ~conn ?override ?note ~action ~errors () =
  let csrf = Csrf.token ~secret conn in
  let err f = Form.field_errors f errors in
  View.page ~title:(if note = None then "New note" else "Edit note")
    [ Form.form ~action ~csrf ?override
        [ Form.field Fields.title ~label:"Title" ~value:(prefill conn note "title" (fun n -> n.title)) ~errors:(err Fields.title);
          Form.field Fields.body ~label:"Body" ~kind:`Textarea ~value:(prefill conn note "body" (fun n -> n.body)) ~errors:(err Fields.body);
          Form.field Fields.priority ~label:"Priority (0–5)" ~kind:`Number
            ~value:(prefill conn note "priority" (fun n -> string_of_int n.priority)) ~errors:(err Fields.priority);
          Form.field Fields.pinned ~label:"Pinned" ~kind:`Checkbox
            ~value:(prefill conn note "pinned" (fun n -> if n.pinned then "on" else ""));
          Form.submit "Save" ] ]

(* ── actions: content-negotiated (HTML for browsers, JSON for Accept: application/json) ── *)
let not_found c = Conn.text ~status:404 c "Not found"

let index c =
  let c, flash = take_flash c in
  Respond.negotiate c ~json:(fun c -> Respond.models c codec (all ())) ~html:(fun c -> View.document c (index_view ~conn:c ?flash (all ())))

let show c =
  match Option.bind (Action.path c "id") find with
  | Some n ->
      let c, flash = take_flash c in
      Respond.negotiate c ~json:(fun c -> Respond.model c codec n) ~html:(fun c -> View.document c (show_view ~conn:c ?flash n))
  | None -> not_found c

let new_ c = View.document c (form_view ~conn:c ~action:"/notes" ~errors:[] ())

(* create/update inject the server-assigned _id into the parsed form (the form never trusts a
   client-supplied id), then validate the WHOLE model — redirect on success (PRG), re-render with
   per-field errors on failure (422) *)
let create c =
  let id = next_id () in
  match Form.parse_assoc codec (("_id", id) :: Conn.body_params c) with
  | Ok n -> put n; Conn.redirect (set_flash c "Note created.") ("/notes/" ^ id)
  | Error errs -> View.document ~status:422 c (form_view ~conn:c ~action:"/notes" ~errors:errs ())

let edit c =
  match Option.bind (Action.path c "id") find with
  | Some n -> View.document c (form_view ~conn:c ~action:("/notes/" ^ n.id) ~override:"PUT" ~note:n ~errors:[] ())
  | None -> not_found c

let update c =
  match Action.path c "id" with
  | Some id when find id <> None -> (
      match Form.parse_assoc codec (("_id", id) :: Conn.body_params c) with
      | Ok n -> put n; Conn.redirect (set_flash c "Note updated.") ("/notes/" ^ id)
      | Error errs -> View.document ~status:422 c (form_view ~conn:c ~action:("/notes/" ^ id) ~override:"PUT" ~errors:errs ()))
  | _ -> not_found c

let destroy c =
  match Action.path c "id" with
  | Some id -> remove id; Conn.redirect (set_flash c "Note deleted.") "/notes"
  | None -> not_found c

(* ── wiring: the security paws (session → method-override → CSRF) then the RESTful 7 ── *)
let mount ep =
  ep
  |> Endpoint.pipe
       [ Session.make ~secret (); Method_override.make (); Csrf.make ~secret () ]
  |> Resource.crud "/notes" ~index ~show ~new_ ~create ~edit ~update ~destroy
