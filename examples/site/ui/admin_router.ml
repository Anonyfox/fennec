(* The admin app's route table — shared, dual-compiled. *)

let routes (r : Fennec_router.Router.t) : Fennec_router.Router.t = r |> Fennec_router.Router.page "/" Page_admin.make
