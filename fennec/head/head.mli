(** Document [<head>] metadata — the pure core. Tag model, dedup+merge
    (inside-out / last-wins), HTML escaping, and rendering to an HTML string.
    Compiles to both targets so the same merge runs on server (SSR) and client
    (CSR) — the server-emitted head equals the client-computed head. *)

type tag =
  | Title of string
  | Charset of string
  | Meta_name of string * string  (** [<meta name content>] (description, …) *)
  | Meta_property of string * string  (** [<meta property content>] (og:*, …) *)
  | Canonical of string  (** [<link rel="canonical" href>] *)
  | Link of (string * string) list  (** arbitrary [<link>] by attrs *)
  | Meta of (string * string) list  (** arbitrary [<meta>] by attrs *)

(** The dedup key for a tag. Single-valued tags collapse to one; repeatable tags
    (e.g. alternate links) are keyed by their identifying attrs. *)
val key : tag -> string

(** Merge tags: last-write-wins per {!key} (== innermost/deepest wins), emitted
    in first-appearance order. Deterministic, so SSR and CSR agree. *)
val merge : tag list -> tag list

(** Render the merged head to an HTML string (native SSR injection). Every value
    is HTML-escaped. *)
val to_html : tag list -> string

(** Escape a value for a double-quoted HTML attribute. *)
val attr_escape : string -> string

(** Escape HTML text content (e.g. inside [<title>]). *)
val text_escape : string -> string

(** Build tags from the [<Head>] component's typed props + an [extra] escape-hatch
    list of raw tags. Shared by both targets so the prop→tag mapping is identical. *)
val of_props :
  ?title:string ->
  ?description:string ->
  ?canonical:string ->
  ?extra:tag list ->
  unit ->
  tag list

(** The merged title, if any (the CSR runtime sets [document.title] from this). *)
val title_of : tag list -> string option
