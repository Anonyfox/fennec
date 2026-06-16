(** Default account emails, rendered with Fur — Fennec's typed twin of Meteor's [Accounts.emailTemplates].

    A {!template} is a function of the configured site name, the recipient address, and the action URL,
    returning a subject + plain-text body + an optional Fur html body. The defaults are clean, email-safe
    Fur components (inline styles, no external CSS); an app overrides any field. This module deliberately
    works on primitives — not the Accounts [user] record — so it sits below accounts core. *)

module Mail = Fennec_mail

type rendered = {
  subject : string;
  text : string;  (** the text/plain body — always sent *)
  html : Fur.vnode option;  (** an optional rich body, rendered to HTML via {!Fur.to_html} *)
}

(** Render the email for one flow, given the site name, the recipient, and the action URL. *)
type template = site_name:string -> email:string -> url:string -> rendered

type templates = {
  site_name : string;
  from : Mail.Address.t;
  verify_email : template;
  reset_password : template;
  enroll_account : template;
  login_code : template;
      (** passwordless sign-in: here [url] carries the one-time CODE (not a link); the default renders
          it as a large monospaced code rather than a button *)
}

(** [default ?site_name ~from ()] is the built-in Fur template set ([site_name] defaults to ["Fennec"]).
    Override fields to customize, e.g. [{ (default ~from ()) with verify_email = my_template }]. *)
val default : ?site_name:string -> from:Mail.Address.t -> unit -> templates

(** [message ~from ~site_name tpl ~email ~url] renders [tpl] into a ready-to-send {!Mail.t} addressed to
    [email] (html via {!Fur.to_html}). *)
val message :
  from:Mail.Address.t -> site_name:string -> template -> email:string -> url:string -> Mail.t
