(* The route table — single source of truth, shared by SSR + client. Routes are
   RELATIVE to the mount base; pages reach params/links via ambient param/p. *)
let router =
  Router.make ~base:"/shop" ~not_found:Not_found.make ()
  |> Router.page "/" Home.make
  |> Router.page "/products" Products.make
  |> Router.page "/products/:id" Product.make
