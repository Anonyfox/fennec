(* The frozen public contract of the Fur runtime core. Abstract types make invalid
   states unrepresentable; this .mli is also the recompile firewall (editing the .ml
   body never recompiles dependents). Internals (reaction, run_effect, the effect
   tracker, flatten/escape, take_seed, Head counter, etc.) are hidden. *)

(* ---- reactivity ---- *)
type 'a signal
val signal : ?eq:('a -> 'a -> bool) -> 'a -> 'a signal
val peek : 'a signal -> 'a
val get : 'a signal -> 'a
val set : 'a signal -> 'a -> unit
val update : 'a signal -> ('a -> 'a) -> unit
val watch : (unit -> unit) -> (unit -> unit)            (* reactive side-effect; returns stop *)
val ( += ) : int signal -> int -> unit
val ( -= ) : int signal -> int -> unit

(* ---- lifecycle ---- *)
val is_browser : bool ref
val on_mount : (unit -> unit) -> unit                   (* browser-only, post-hydration *)
val on_cleanup : (unit -> unit) -> unit                 (* on owning component's unmount *)
val flush_mounts : unit -> unit

(* ---- ambient event (read inside handlers) + browser facade (SSR-safe) ---- *)
val target_value : unit -> string
val target_checked : unit -> bool
val key : unit -> string
val prevent_default : unit -> unit
module Browser : sig
  val local_get : string -> string option
  val local_set : string -> string -> unit
  val local_remove : string -> unit
end

(* ---- vdom ---- *)
type vnode
type attr
val text : string -> vnode
val raw : string -> vnode                               (* verbatim markup (templates/head) *)
val frag : vnode list -> vnode
val h : ?key:string -> string -> attr list -> vnode list -> vnode
val comp : cid:string -> ?key:string -> (unit -> (unit -> vnode)) -> vnode
val on : string -> (unit -> unit) -> attr
val attr : string -> string -> attr
val class_ : string -> attr
val node : 'a -> vnode                                  (* child coercion: int|float|string|vnode *)
val skey : 'a -> string                                 (* key coercion: int|string *)
val each : 'a list -> ('a -> 'b) -> 'b list
val with_key : string -> vnode -> vnode
val to_html : vnode -> string                           (* SSR *)
val document : vnode -> string                          (* SSR with <!doctype> *)

(* ---- head ---- *)
module Head : sig
  type tag =
    | Title of string
    | Meta of (string * string) list
    | Link of (string * string) list
    | Script of (string * string) list * string
    | Json_ld of string
  module Tag : sig
    val title : string -> tag
    val meta : name:string -> string -> tag
    val og : string -> string -> tag
    val link : rel:string -> ?attrs:(string * string) list -> string -> tag
    val script : ?attrs:(string * string) list -> ?body:string -> unit -> tag
    val json_ld : string -> tag
  end
  val sources : (int * tag list) list signal
  val use : (unit -> tag list) -> unit                  (* dynamic, multi-tag *)
  val title : string -> unit
  val description : string -> unit
  val meta : name:string -> string -> unit
  val og : string -> string -> unit
  val link : rel:string -> ?attrs:(string * string) list -> string -> unit
  val json_ld : string -> unit
  val tag_key : tag -> string
  val resolve : (int * tag list) list -> tag list
  val to_ssr : unit -> string
end

(* ---- data ---- *)
module Data : sig
  type 'a t
  val seed : (string, string) Hashtbl.t                 (* per-request context (server) / seed (client) *)
  val put_seed : string -> string -> unit
  val clear_seed : unit -> unit
  val source : (string -> (string -> unit) -> unit) ref (* platform/app fetch strategy *)
  val resource : key:string -> ?client_only:bool -> fallback:'a -> decode:(string -> 'a) -> unit -> 'a t
  val string : string -> ?fallback:string -> ?client_only:bool -> unit -> string t
  val value : 'a t -> 'a
  val loading : 'a t -> bool
  val refetch : 'a t -> unit
  val js_string : string -> string
  val to_script : unit -> string
end

(* ---- routing ---- *)
module Matcher : sig
  type params = (string * string) list
  val segments : string -> string list
  val match_one : pattern:string -> string -> params option
  val find : (string * 'a) list -> string -> ('a * params) option
  val param : params -> string -> string option
end
module Router : sig
  type t
  type page = unit -> (unit -> vnode)
  val make : ?base:string -> ?not_found:page -> unit -> t
  val page : ?name:string -> string -> page -> t -> t
  val base : t -> string
  val relativize : string -> string -> string
  val absolutize : string -> string -> string
  val current_path : t -> string
  val set_path : t -> string -> unit
  val build : t -> string -> (string * string) list -> string
  val href : t -> string -> (string * string) list -> string
  val path : t -> ('a, unit, string) format -> 'a
  val ext : ('a, unit, string) format -> 'a
  val navigate : string -> unit
  val sync_path : string -> unit
  val outlet : t -> vnode
  val activate : t -> unit
  val current : unit -> t
  val param : string -> string option
  val param_or : string -> string -> string
end

(* ---- ambient router helpers (the active app) ---- *)
val p : ('a, unit, string) format -> 'a                 (* in-app typed path *)
val ext : ('a, unit, string) format -> 'a               (* outer-reach raw url *)
val href : string -> (string * string) list -> string
val param : string -> string option
val param_or : string -> string -> string
val navigate : string -> unit
val sync_path : string -> unit
val outlet : unit -> vnode

(* ---- document shell ---- *)
module Doc : sig
  type ctx = { head : string; data : string; body : string; styles : string; client_js : string }
  val head : ctx -> vnode
  val styles : ctx -> vnode
  val data : ctx -> vnode                                 (* fast-render seed <script> only *)
  val outlet : ctx -> vnode
  val scripts : ctx -> vnode                              (* seed + client bundle, both inline *)
end

(* ---- a mounted app (the generator emits a mount list) ---- *)
type mount = {
  base : string;
  root : unit -> (unit -> vnode);
  router : Router.t;
  document : Doc.ctx -> vnode;
}

(* ---- the reconciler, parameterized over a DOM backend ---- *)
module type BACKEND = sig
  type node
  val create_text : string -> node
  val create_element : string -> node
  val get_text : node -> string
  val set_text : node -> string -> unit
  val get_attr : node -> string -> string option
  val set_attr : node -> string -> string -> unit
  val remove_attr : node -> string -> unit
  val set_prop : node -> string -> string -> unit
  val get_prop : node -> string -> string
  val append : node -> node -> unit
  val remove : node -> node -> unit
  val replace : node -> node -> node -> unit
  val parent : node -> node option
  val listen : node -> string -> (unit -> unit) ref -> unit
  val child : node -> int -> node option
  val first_child : node -> node option
end
module Reconcile : functor (B : BACKEND) -> sig
  val mount_root : B.node -> (unit -> vnode) -> unit
end
