(* A Browser suite for `fennec test browser`. No base_url — the harness boots a dedicated
   isolated app instance per suite and hands us its URL via FENNEC_TEST_URL. Each [test] runs
   in its own fresh isolated browser context (own cookies/localStorage), so they're
   independent and the runner can fan them out in parallel. *)
open Fennec_hunt.Live

(* wait for the Fur client to finish hydrating — an app-agnostic signal set by Fur_csr,
   awaited evented (one round-trip) like every other condition *)
let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let () = test "home page server-renders + hydrates" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> expect_text "h1" "Welcome to the Fennec site"
  |> expect_text ".greeting .msg" "Hello from the server"
  |> ignore

let () = test "local signal state (counter increments/decrements)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> click ".cbtn.inc" |> expect_text ".count" "1"
  |> click ".cbtn.inc" |> expect_text ".count" "2"
  |> click ".cbtn.dec" |> expect_text ".count" "1"
  |> ignore

let () = test "SPA navigation (client-side, no full reload)" @@ fun page ->
  page
  |> goto "/" |> hydrated
  |> eval "window.__spa = 1"
  |> click ".nav-link[href=\"/products\"]"
  |> expect_url "/products"
  |> expect_js ~descr:"window marker survived (no full reload happened)" "window.__spa===1"
  |> ignore

(* run every registered test; base_url comes from FENNEC_TEST_URL (set per-suite by `fennec
   test`), --headed/--screenshots/--grep/--jobs/--reporter are passed through as argv *)
let () = Fennec_hunt.Run.main_cli ()
