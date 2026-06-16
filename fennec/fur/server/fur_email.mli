(** Email CSS inlining — flatten precompiled component styles into [style=] attributes.

    Email clients (Outlook especially) ignore [<style>]/external sheets, so component CSS must be inlined.
    The CSS work happens at BUILD time: [style_extract] emits a list of [(scope, selector-key, decls)]
    rules from the [%%style] blocks. This module is the pure-OCaml RUNTIME apply — no CSS parser, no Rust:
    walk the vnode and, for each element, merge the declarations that match its [data-fur] scope and its
    classes/tag (exactly the scoping the Fur ppx stamps). So the same [%%style] component — minus JS —
    renders as an email with its styles inlined. *)

(** A precompiled email stylesheet: a lookup from [(scope, selector-key)] to CSS declarations. *)
type t

(** Build a stylesheet from the precompiled rule list [(scope, selector-key, declarations)] — the [inline]
    artifact [style_extract] emits (one entry per simple [%%style] rule: a single class or a bare tag).
    Rules sharing a key are concatenated in order (the cascade). *)
val of_rules : (string * string * string) list -> t

(** [to_email t ?tokens v] renders [v] to HTML with [t]'s matching declarations inlined into each
    element's [style] attribute. Matching is by the element's [data-fur] scope and its classes/tag; class
    rules win over tag rules, and an explicit inline [style=] wins over the stylesheet. [tokens] maps a
    custom-property name (with its leading [--]) to a literal, e.g. [[ ("--brand", "#e8590c") ]], so
    [var(--brand)] resolves for clients (Outlook) that can't. JS handlers are dropped (email has none). *)
val to_email : t -> ?tokens:(string * string) list -> Fur.vnode -> string
