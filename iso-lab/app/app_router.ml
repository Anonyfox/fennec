(* The router INSTANCE (base + not_found). Holds no page routes itself — those are
   registered in routes.ml. Kept separate so pages can reference this instance for
   typed path building without a dependency cycle (routes.ml imports the pages). *)
open Iso
let router = Router.make ~base:"/shop" ~not_found:Not_found.make ()

(* Typed, base-aware path builders for THIS app (eta-expanded so they stay
   polymorphic over the format). [p] for in-app links (base-prefixed, route-checked);
   [ext] for outer reach (raw url to other apps / external). *)
let p fmt = Router.path router fmt
let ext fmt = Router.ext fmt
