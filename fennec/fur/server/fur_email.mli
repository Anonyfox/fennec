(** Email CSS inlining — flatten precompiled component styles into [style=] attributes.

    Email clients (Outlook especially) ignore [<style>]/external sheets and [var()] custom properties, so
    component CSS must be inlined and resolved. ALL of that happens at BUILD time: [style_extract] reads
    the [%%style] blocks, resolves [var(--brand)] against the app's [:root], and emits a flat
    [(scope, selector-key, decls)] list — AND a line that {!install}s it as the ambient stylesheet. So this
    module is the pure-OCaml RUNTIME apply: no CSS parser, no Rust, no tokens. A [%%style] component, minus
    JS, renders as an email with [to_email] — and userland passes nothing but the component. *)

(** A precompiled email stylesheet: a lookup from [(scope, selector-key)] to CSS declarations. *)
type t

(** Build a stylesheet from a precompiled rule list — exposed for tests/overrides; normal code uses the
    ambient one (see {!install}/{!to_email}). Rules sharing a key are concatenated in order (the cascade). *)
val of_rules : (string * string * string) list -> t

(** Install [rules] as the AMBIENT email stylesheet. The generated [Site_styles] calls this at link time
    (the app already links it for the web [~styles]), so {!to_email} needs no userland wiring. Last wins. *)
val install : (string * string * string) list -> unit

(** [to_email ?stylesheet v] renders [v] to HTML with the stylesheet's matching declarations inlined into
    each element's [style] attribute — matched by the element's [data-fur] scope and its classes/tag (class
    beats tag, and an explicit inline [style=] wins). With no [?stylesheet] the ambient one (installed by
    [Site_styles]) is used; with none installed it is plain {!Fur.to_html}. [var()] was already resolved at
    build time, so there is nothing to configure: pass the component, nothing else. *)
val to_email : ?stylesheet:t -> Fur.vnode -> string
