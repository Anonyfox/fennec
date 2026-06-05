(* The site's end-to-end suite, written with the fennec.e2e DSL — the same behaviours the
   old hand-rolled CDP script covered, now as readable top-to-bottom pipes. Each [test]
   runs in its own fresh isolated browser context (own cookies/localStorage), so they are
   independent and the runner can fan them out in parallel.

   The harness sets base_url to the web app (http://localhost:8020); a leading-'/' path is
   resolved against it, an absolute URL (the admin app) is used as-is. *)
open Fennec_e2e.Live

(* referenced by run.ml so this module (and thus its test registrations below) is linked *)
let load = ()

let admin = "http://localhost:8022"

(* wait for the Fur client to finish hydrating — an app-agnostic signal set by Fur_csr,
   awaited evented (one round-trip) like every other condition *)
let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let () = test "hydration + isomorphic data (fast-render seed)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> expect_text ".greeting .msg" "Hello from the server"
  |> expect_text ".greeting .gstatus" "(ready)"     (* seeded → ready immediately, no flash *)
  |> expect_text ".bdata" "fetched live in the browser"  (* client-only data, fetched post-hydration *)
  |> expect_text ".mounted" "mounted"
  |> ignore

let () = test "local signal state (counter increments/decrements)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> click ".cbtn.inc" |> expect_text ".count" "1"
  |> click ".cbtn.inc" |> expect_text ".count" "2"
  |> click ".cbtn.dec" |> expect_text ".count" "1"
  |> ignore

let () = test "localStorage persists across reloads (Browser facade)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> expect_text ".visits" "visits (localStorage): 1"
  |> goto "/" |> hydrated
  |> expect_text ".visits" "visits (localStorage): 2"
  |> ignore

let () = test "data refetch hits the network and resolves" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> click "#refetch"
  |> expect_text ".greeting .gstatus" "(ready)"
  |> expect_text ".greeting .msg" "Hello from the server"
  |> ignore

let () = test "SPA navigation (client-side, no full reload)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> eval "window.__spa = 1"
  |> click ".nav-link[href=\"/products\"]"
  |> expect_url "/products"
  |> expect_text ".stats" "todos in store"
  |> expect_js ~descr:"window marker survived (no full reload happened)" "window.__spa===1"
  |> ignore

let () = test "global store + controlled form + keyed list" @@ fun page ->
  page
  |> goto "/products" |> hydrated
  |> expect_text ".stats" "todos in store: 0"
  |> type_enter "#todo-input" "milk"
  |> expect_text ".stats" "todos in store: 1"
  |> expect_text ".todo-items" "milk"
  |> fill "#todo-input" "eggs" |> click "#add"
  |> expect_text ".stats" "todos in store: 2"
  |> click ".todo .rm"
  |> expect_text ".stats" "todos in store: 1"
  |> ignore

let () = test "dynamic route param via typed path link" @@ fun page ->
  page
  |> goto "/products" |> hydrated
  |> click ".p7"
  |> expect_url "/products/7"
  |> expect_text "h1" "Product #7"
  |> ignore

let () = test "catch-all renders not_found with the unmatched path" @@ fun page ->
  page
  |> goto "/nope/xyz" |> hydrated
  |> expect_text ".missing" "no route: /nope/xyz"
  |> ignore

let () = test "per-app bundle isolation: web bundle excludes admin code" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> expect_js ~descr:"web bundle does not contain 'admin actions'"
       "(async()=>!(await (await fetch('/_apps/web/main.js')).text()).includes('admin actions'))()"
  |> ignore

let () = test "admin app: separate endpoint, whitelabel, shared component hydrates" @@ fun page ->
  page
  |> goto (admin ^ "/") |> hydrated
  |> expect_js ~descr:"admin whitelabel body class" "document.body.className.includes('admin')"
  |> click ".cbtn.inc" |> expect_text ".count" "1"
  |> expect_js ~descr:"admin bundle contains 'admin actions'"
       "(async()=>(await (await fetch('/_apps/admin/main.js')).text()).includes('admin actions'))()"
  |> ignore

(* ---- forced-race stress: the patterns that previously flaked. These hammer navigation +
   execution-context swaps; with loaderId-matched loads + context-pinned evals they must be
   deterministic, every run. ---- *)

let () = test "STRESS: 6 rapid reloads keep localStorage + hydration consistent" @@ fun page ->
  let p = page |> goto "/" |> hydrated |> expect_text ".visits" "visits (localStorage): 1" in
  let final =
    List.fold_left
      (fun p n -> p |> goto "/" |> hydrated |> expect_text ".visits" (Printf.sprintf "visits (localStorage): %d" n))
      p [ 2; 3; 4; 5; 6 ]
  in
  ignore final

let () = test "STRESS: assert immediately after navigation (no settle), many pages" @@ fun page ->
  page
  |> goto "/products" |> expect_text "h1" "Products"
  |> goto "/about" |> expect_text "h1" "About"
  |> goto "/" |> expect_text "h1" "Welcome to the Fennec site"
  |> goto "/products/7" |> expect_text "h1" "Product #7"
  |> goto "/nope/zzz" |> expect_text ".missing" "no route: /nope/zzz"
  |> goto "/" |> expect_text "h1" "Welcome to the Fennec site"
  |> ignore




(* streaming responses (server send_chunked / send_file) proven over a real fetch *)
let () = test "send_chunked streams + the client reassembles the chunks" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> eval "fetch('/api/stream').then(r => r.text()).then(t => { window.__stream = t })"
  |> wait_for ~descr:"chunked body reassembled" "window.__stream === 'chunk-1chunk-2chunk-3'"
  |> ignore

let () = test "send_file streams a file body with the right bytes" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> eval "fetch('/api/download').then(r => r.text()).then(t => { window.__dl = t })"
  |> wait_for ~descr:"downloaded file body" "window.__dl === 'hello from send_file'"
  |> ignore
