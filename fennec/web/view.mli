(** Server-rendered HTML views. A "dead view" is just a {!Fur} component tree rendered to a string
    with NO client bundle and no hydration — [Fur.to_html] strips event handlers and runs each
    component's setup once. It is the SAME component language as the live isomorphic apps, so a dead
    view upgrades to interactive later by adding a client boot, with no rewrite.

    {[ let show conn post =
         View.document conn
           (View.page ~title:post.title
              [ Fur.h "article" [] [ Fur.h "h1" [] [ Fur.text post.title ]; Fur.h "p" [] [ Fur.text post.body ] ] ]) ]} *)

module Conn = Fennec_paw.Conn

(** Respond with a vnode FRAGMENT as the body (no doctype/shell) — for partials / progressive swaps. *)
val html : ?status:int -> Conn.t -> Fur.vnode -> Conn.t

(** Respond with a FULL document (prepends [<!doctype html>]) — for a top-level page. *)
val document : ?status:int -> Conn.t -> Fur.vnode -> Conn.t

(** A conventional [<html>/<head>/<body>] shell: charset + responsive viewport + [title], optional
    inlined [styles] and extra [head] nodes, then [body]. Pass the result to {!document}. *)
val page : ?lang:string -> ?title:string -> ?head:Fur.vnode list -> ?styles:string -> Fur.vnode list -> Fur.vnode
