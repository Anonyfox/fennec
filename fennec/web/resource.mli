(** RESTful resources — the convention. One {!crud} call wires the 7 actions, installs the security
    paws (session → method-override → CSRF), auto-loads the [:id] entity (404 if absent), owns the
    server-assigned id, and auto-parses + validates the submitted form into the typed model
    (re-rendering the form with per-field errors on failure). HTML only — JSON APIs are hand-built
    with the {!Action}/{!Respond} building blocks.

    {[ open Fennec.Fur
       let form c note = page ~title:"Note" [ Form.view c Note.codec ~action:"/notes" ?entity:note
                                                (fun f -> [ Form.bound f Note.Fields.title ~label:"Title"; Form.submit "Save" ]) ]
       let mount ep =
         resource Note.codec "/notes" ~secret ~load:Notes.find ~form
           ~index:(fun c   -> respond c (View.index (Notes.all ())))
           ~show:(fun  c n -> respond c (View.show n))
           ~create:(fun c n -> ignore (Notes.create n); redirect c "/notes" ~flash:"Created")
           ~update:(fun c n -> Notes.save n; redirect c ("/notes/" ^ n.id) ~flash:"Updated")
           ~destroy:(fun c n -> Notes.remove n.id; redirect c "/notes" ~flash:"Deleted")
           ep ]} *)

module Endpoint = Fennec_server.Endpoint
module Conn = Fennec_paw.Conn

(** Answer with an HTML page (renders the vnode to the response). *)
val respond : ?status:int -> Conn.t -> Fur.vnode -> Conn.t

(** Redirect, optionally flashing a message into the session for the next page (post-redirect-get). *)
val redirect : ?flash:string -> Conn.t -> string -> Conn.t

(** Read + clear the flash set by a prior {!redirect} (returns the updated conn and the message). *)
val flash : Conn.t -> Conn.t * string option

(** Wire a RESTful resource for [base] onto the endpoint and install its security paws. [load] maps a
    path id to the entity (member actions 404 when [None]); [form] renders the new/edit form (and is
    re-rendered with errors on a failed write); [create]/[update] receive the already-parsed,
    validated model (the server-owned id is injected — [~new_id] overrides the default generator);
    handlers answer via {!respond}/{!redirect}. [new] and [edit] are derived from [form] + [load].
    [secret] signs the session + CSRF. *)
val crud :
  'a Codec.t ->
  string ->
  secret:string ->
  ?new_id:(unit -> string) ->
  load:(string -> 'a option) ->
  form:(Conn.t -> 'a option -> Fur.vnode) ->
  index:(Conn.t -> Conn.t) ->
  show:(Conn.t -> 'a -> Conn.t) ->
  create:(Conn.t -> 'a -> Conn.t) ->
  update:(Conn.t -> 'a -> Conn.t) ->
  destroy:(Conn.t -> 'a -> Conn.t) ->
  Endpoint.t ->
  Endpoint.t
