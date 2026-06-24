(** [fennec new NAME] — scaffold a minimal, WORKING Fennec web app.

    Emits a self-contained dune project (its own [dune-project] carrying the [(dialect (name mlx) …)]
    stanza + the [fennec-cli]/[mlx] deps, a [server.ml] that renders a [.mlx] component to HTML via
    [Fennec.Fur.Handler], and a starter [web/hello.mlx]) so that immediately after:

    {[ cd NAME && dune build && fennec dev ]}

    the app builds, serves server-rendered HTML, and the editor lights up on the [.mlx]. The scaffold
    is SSR-only (a plain Paw route renders the component) — no client bundle / route_gen wiring, which
    keeps the generated project tiny and dependency-light; the [examples/site] tree shows the full
    isomorphic SPA setup when one is wanted.

    The file SET is pure (so it is unit-tested without touching disk); {!run} writes it. *)

(** Sanitize a user-supplied project name into a valid OCaml library name: lowercase, non-alphanumeric
    runs collapsed to [_], a leading digit prefixed with [_]. ["my-cool-app"] becomes ["my_cool_app"].
    Pure; exposed for testing. *)
val lib_name_of : string -> string

(** The (relative-path, contents) files a scaffold named [name] consists of. Pure and deterministic —
    the unit tests assert the generated [dune-project] carries the dialect stanza and the deps, and
    that [server.ml] + the [.mlx] are present. *)
val files : name:string -> (string * string) list

(** Scaffold a new app named [name] in [in_dir] (default: the cwd). Creates [in_dir/name/…], refusing
    to write into a non-empty existing directory. Prints what it wrote + the next steps. Returns a
    process exit code (0 on success, 1 on a bad name or an occupied target). *)
val run : ?in_dir:string -> string -> int
