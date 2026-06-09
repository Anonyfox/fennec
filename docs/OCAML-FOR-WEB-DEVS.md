# New to OCaml? A 5-minute orientation for web devs

You don't need to be an OCaml expert to build with fennec. If you've written React, Rails, or
Phoenix, you already know the shapes — here's the translation.

## The mental map

| You know | In fennec / OCaml | Notes |
|---|---|---|
| `npm` / `cargo` | **opam** | the package manager |
| `vite` / `webpack` | **dune** | the build tool — fast, incremental, watches for you |
| `package.json` + `tsconfig` | **`dune-project`** + per-dir **`dune`** files | small, declarative |
| JSX / `.tsx` | **`.mlx`** | OCaml with HTML-like markup (a ppx, not a separate language) |
| `useState` / Solid signals | **`signal`** | reactive local state — `signal 0`, `get s`, `s += 1` |
| Redux / a global store | shared **signals** in a `store/` module | plain values, reactive everywhere |
| Express / Plug / Rack middleware | **paws** (`conn -> conn`) | compose a pipeline ([`PAW.md`](./PAW.md)) |
| Next.js / Remix SSR + hydration | **Fur** | server-renders to HTML, hydrates in the browser — *one* source, no Node |
| a `.d.ts` declaration | a **`.mli`** interface file | the explicit public surface of a module |
| TypeScript types | OCaml types | stronger: full inference, no `null`/`undefined`, exhaustive matches |

## The 20% of the syntax you'll actually use

```ocaml
let greet name = "hi " ^ name        (* function; ^ concatenates strings *)
let x = 1 and y = 2 in x + y          (* local bindings *)

(* a record (like an object/struct) *)
type user = { name : string; admin : bool }
let u = { name = "ada"; admin = true }
u.name                                (* field access *)

(* a variant (like a tagged union / enum) + pattern match (like switch, but exhaustive) *)
type role = Guest | Member | Admin
let label = function Guest -> "?" | Member -> "m" | Admin -> "a"

(* options instead of null — the compiler forces you to handle "nothing" *)
let first = function [] -> None | x :: _ -> Some x
```

- **No `null`.** "Maybe a value" is `'a option` (`Some x` / `None`); the compiler won't let you forget
  the `None` case. This kills a whole class of runtime crashes.
- **`match … with`** is `switch` that the compiler checks for completeness.
- **`|>`** pipes left-to-right: `x |> f |> g` = `g (f x)` — you'll see it everywhere.
- **Whitespace-insensitive**, expression-oriented; the last expression is the return value.

## A component, annotated

```ocaml
(* counter.mlx *)
let make () =
  let count = signal 0 in                          (* reactive state, like useState(0) *)
  fun () ->                                          (* the render function *)
    <button onClick=(count += 1)>(get count)</button>
```

The server runs `make` to produce HTML; the browser re-runs it to hydrate and stays live. Same file,
both places.

## You're set

That's enough to read [`QUICKSTART.md`](./QUICKSTART.md) and the [example](../examples/site/README.md).
You'll pick up the rest from the editor — Merlin/LSP gives types-on-hover and completion, and the
compiler's error messages are precise. Lean on both; you don't have to hold it all in your head.
