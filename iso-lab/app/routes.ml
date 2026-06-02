(* Registers the route table into the shared instance. The single source of truth
   for paths; shared by SSR and the client. Routes are RELATIVE to the mount base. *)
open Iso
let router = App_router.router
let () =
  ignore
    (router
    |> Router.page ~name:"home" "/" Home.make
    |> Router.page ~name:"products" "/products" Products.make
    |> Router.page ~name:"product" "/products/:id" Product.make)
