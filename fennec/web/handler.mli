(** The handler response side — render a {!Fur} component tree to an HTML response through the SAME SSR
    engine the SPA apps use (Head metadata + scoped [%%style] + a document template), but STATIC: no
    fast-render seed, no client bundle, no hydration — just server-rendered HTML, instantly. Plus the
    response niceties (redirect + flash, fragment) and the hidden-field helpers a hand-authored [.mlx]
    form needs. A handler is a [Conn.t -> Conn.t] that reads typed inputs ({!Form}/{!Action}), runs
    its Pulse/Accounts logic, and answers with one of these.

    {[ let get conn = Handler.html conn <main className="page"><Hero/> <Contact_form/></main>
       let post conn = match Form.read Message.codec conn with
         | Ok m -> ignore (Messages.create m); Handler.redirect conn "/thanks" ~flash:"Sent!"
         | Error errs -> Handler.html ~status:422 conn (Contact_form.view ~errors:errs ()) ]} *)

module Conn = Fennec_paw.Conn

(** A minimal static document shell (head + scoped styles + body; no seed, no bundle). *)
val default_template : Fur.Doc.ctx -> Fur.vnode

(** Render a component tree to a full HTML response, STATIC (no JS/hydration). [~head] appends to the
    resolved Head metadata; [~styles] inlines scoped CSS; [~template] overrides the document shell. *)
val html :
  ?status:int -> ?styles:string -> ?head:string -> ?template:(Fur.Doc.ctx -> Fur.vnode) -> Conn.t -> Fur.vnode -> Conn.t

(** Answer a vnode FRAGMENT (no document shell) — partials / progressive swaps. *)
val fragment : ?status:int -> Conn.t -> Fur.vnode -> Conn.t

(** Redirect, optionally flashing a message into the session for the next page (post-redirect-get). *)
val redirect : ?flash:string -> Conn.t -> string -> Conn.t

(** Read + clear the flash set by a prior {!redirect}. *)
val flash : Conn.t -> Conn.t * string option

(** A CSRF hidden field for a hand-authored form (token from the conn; pair with [Paw.Csrf.make]).
    Empty when CSRF isn't active, so it's harmless to always include. *)
val csrf_field : Conn.t -> Fur.vnode

(** A method-override hidden field so an HTML form can drive PUT/PATCH/DELETE (pair with
    [Paw.Method_override]). *)
val method_field : string -> Fur.vnode
