(* Client entry for the /greet PAGE — its own isolated jsoo bundle that boots ONLY this page's view
   (the [%%conn]-stripped half from ../pages_client). SSR seed -> hydrate -> interactive + live data. *)
let () = Fur_csr.start_page Site_pages_client.Greet_page.page
