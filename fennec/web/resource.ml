(* RESTful resources — the convention. ONE [crud] call wires the 7 actions, installs the security
   paws (session → method-override → CSRF), auto-loads the [:id] entity (404 if absent), owns the
   server-assigned id, and auto-parses + validates the submitted form into the typed model
   (re-rendering the form with per-field errors on failure). Handlers are Paw-native — they take the
   Conn (the request context) plus, for member/write actions, the typed entity, and answer it via
   {!respond} / {!redirect}. HTML only: JSON APIs are hand-built with the {!Action} / {!Respond}
   building blocks — different job, not mixed in here. *)

module Endpoint = Fennec_server.Endpoint
module Conn = Fennec_paw.Conn
module Session = Fennec_server.Session
module Csrf = Fennec_server.Csrf
module Method_override = Fennec_server.Method_override

let id_wire = "_id" (* the doc_id convention — the field the resource owns, never in the form *)

(* a default server id (24 hex, ObjectId-shaped) when the caller doesn't supply [~new_id] *)
let _seq = ref 0

let default_new_id () =
  incr _seq;
  String.sub (Digest.to_hex (Digest.string (Printf.sprintf "%f-%d" (Unix.gettimeofday ()) !_seq))) 0 24

(* answer with a page (HTML) *)
let respond ?status c v = View.document ?status c v

(* redirect, optionally flashing a message into the session for the next page (post-redirect-get) *)
let redirect ?flash c url =
  let c = match flash with Some m -> Session.set c "flash" m | None -> c in
  Conn.redirect c url

(* read + clear the flash set by a prior {!redirect} *)
let flash c = match Session.get c "flash" with Some m -> (Session.delete c "flash", Some m) | None -> (c, None)

let crud (type a) (codec : a Codec.t) (base : string) ~secret ?(new_id = default_new_id)
    ~(load : string -> a option) ~(form : Conn.t -> a option -> Fur.vnode) ~(index : Conn.t -> Conn.t)
    ~(show : Conn.t -> a -> Conn.t) ~(create : Conn.t -> a -> Conn.t) ~(update : Conn.t -> a -> Conn.t)
    ~(destroy : Conn.t -> a -> Conn.t) (ep : Endpoint.t) : Endpoint.t =
  let not_found c = Conn.text ~status:404 c "Not found" in
  (* a member action: load the :id entity or 404 *)
  let member f c = match Option.bind (Conn.path_param c "id") load with Some e -> f c e | None -> not_found c in
  (* parse the form into the model with the server-owned [id] injected; on success run [ok], on
     failure re-render the form (errors stashed on the conn → the form shows them, body repopulates) *)
  let parsed id ok c =
    match Form.parse codec c ~inject:[ (id_wire, id) ] with
    | Ok n -> ok c n
    | Error errs -> respond ~status:422 c (form (Form.put_errors c errs) None)
  in
  let new_h c = respond c (form c None) in
  let edit_h = member (fun c e -> respond c (form c (Some e))) in
  let create_h c = parsed (new_id ()) create c in
  let update_h c =
    match Conn.path_param c "id" with Some id when load id <> None -> parsed id update c | _ -> not_found c
  in
  ep
  |> Endpoint.pipe [ Session.make ~secret (); Method_override.make (); Csrf.make ~secret () ]
  |> Endpoint.get base index
  |> Endpoint.get (base ^ "/new") new_h (* before /:id so "new" isn't read as an id *)
  |> Endpoint.post base create_h
  |> Endpoint.get (base ^ "/:id/edit") edit_h
  |> Endpoint.get (base ^ "/:id") (member show)
  |> Endpoint.put (base ^ "/:id") update_h
  |> Endpoint.patch (base ^ "/:id") update_h
  |> Endpoint.delete (base ^ "/:id") (member destroy)

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

type todo = { id : string; title : string }

let todo_codec =
  Codec.(seal (record (fun id title -> { id; title }) |> field doc_id (fun t -> t.id) |> field (req "title" (non_empty string)) (fun t -> t.title)))

let store : (string, todo) Hashtbl.t = Hashtbl.create 8
let secret = "resource-test-secret-key-1234567890"

let sample () =
  Hashtbl.reset store;
  Endpoint.make ~name:"t" ()
  |> crud todo_codec "/todos" ~secret ~load:(Hashtbl.find_opt store)
       ~form:(fun _ _ -> Fur.h "form" [] [ Fur.text "form" ])
       ~index:(fun c -> respond c (Fur.text "index"))
       ~show:(fun c t -> respond c (Fur.text ("show:" ^ t.title)))
       ~create:(fun c t -> Hashtbl.replace store t.id t; redirect c ("/todos/" ^ t.id))
       ~update:(fun c t -> Hashtbl.replace store t.id t; redirect c ("/todos/" ^ t.id))
       ~destroy:(fun c t -> Hashtbl.remove store t.id; redirect c "/todos")

let status meth ?(headers = []) ?(body = "") path =
  let h = Endpoint.handler (sample ()) in
  (Fennec_paw.Paw.run h (H.make_request ~meth ~path ~headers ~body ())).Fennec_core.Http.status

(* GET routes are testable in-process; write actions go through the auto-installed CSRF paw, so they
   need the token+cookie dance — covered by the http e2e (examples/site/test/http/notes_test.ml). *)
let%test "crud GET routes resolve with conventional precedence + member 404" =
  status H.GET "/todos" = 200
  && status H.GET "/todos/new" = 200 (* literal beats /:id *)
  && status H.GET "/todos/missing" = 404 (* member load → 404 (CSRF doesn't gate GET) *)

let%test "writes are CSRF-gated by default: a tokenless POST is rejected" =
  status H.POST ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"title=x" "/todos" = 403
