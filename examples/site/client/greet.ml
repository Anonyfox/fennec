(* Client entry for the /greet PAGE — its own isolated jsoo bundle that boots ONLY this page's view
   (no router, no sibling app code). SSR seed -> hydrate -> interactive. *)
let () = Fur_csr.start_page Greet_page.page
