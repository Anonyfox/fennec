(** Detecting served-asset changes for livereload.

    dune rewrites the whole web root on every build (fresh mtimes), so we compare CONTENT
    hashes — a real change versus churn — and distinguish a CSS-only edit (hot-swap, no reload)
    from a JS/other one (full reload). *)

(** What changed in the web root since the last {!poll}. *)
type change = Nothing | Css_only | Reload

(** Map "did any css change / any non-css change" to a {!change}. A non-css change always wins
    (a reload). Pure. *)
val classify : css:bool -> other:bool -> change

(** Tracks the content hashes of the web root so {!poll} can distinguish a
    real change from dune's mtime-churn and classify CSS-only vs full-reload. *)
type t

(** Track the [.css]/[.js]/[.mjs] files under [dir]. *)
val create : dir:string -> t

(** Rescan and report what changed since the previous poll (updating the baseline). *)
val poll : t -> change

(** Reset the baseline to the current tree, so the next {!poll} reports {!Nothing}. *)
val seed : t -> unit
