(* The web app's route table — SHARED, compiled to both targets. The SAME
   Router.make/page calls run native (SSR) and melange (CSR), so server and client
   agree on path -> page. The layout is supplied per-target at mount time (SSR
   only), so this file is layout-agnostic and dual-compiles cleanly. *)

let routes (r : Fennec_router.Router.t) : Fennec_router.Router.t =
  r
  |> Fennec_router.Router.page "/" Page_home.make
  |> Fennec_router.Router.page "/about" Page_about.make
