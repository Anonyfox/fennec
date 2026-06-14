(* The /greet PAGE end-to-end in a real browser — a standalone single-page SPA with its OWN isolated
   jsoo bundle (no router). Proves the cross-stage boundary: the server computes the payload, seeds
   it, and the page SSRs it (the seeded text is present pre-JS) and then hydrates into something
   ALIVE. The strongest proof is the live Counter: it STARTS at the server-computed count — the
   cross-stage value became interactive state, not just static text. Same SSR + hydration machinery
   an app uses — just one view, no client router. *)
open Fennec_hunt.Live

let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let%browser "page: server-seeded payload SSRs, then the page's own bundle hydrates it into live state" =
 fun page ->
  page
  |> goto "/greet?name=Ada"
  |> expect_text "#greet h1" "Hello, Ada!" (* the cross-stage payload, server-computed, present pre-JS *)
  |> expect_text "#greet .seed" "server-computed start = 3" (* count = String.length "Ada" *)
  |> hydrated (* the page's OWN bundle boots — no app router involved *)
  |> expect_text "#greet .count" "3" (* the live counter STARTS at the seeded count: seed -> behaviour *)
  |> click ".cbtn.inc"
  |> expect_text "#greet .count" "4" (* alive: local signal state, seeded then driven client-side *)
  |> click ".cbtn.inc"
  |> expect_text "#greet .count" "5"
  |> click ".cbtn.dec"
  |> expect_text "#greet .count" "4"
  |> ignore

let%browser "page: default payload when no query param" = fun page ->
  page
  |> goto "/greet"
  |> expect_text "#greet h1" "Hello, world!"
  |> hydrated
  |> expect_text "#greet .count" "5" (* String.length "world" = 5, seeded into the live counter *)
  |> ignore

let%browser "page: live data RECONSTRUCTS via Pulse after hydration (seed static, reconstruct live)" =
 fun page ->
  page
  |> goto "/greet?name=Ada"
  |> expect_text "#greet .seed" "server-computed start = 3" (* the SEEDED static payload, pre-JS *)
  |> hydrated
  (* the live half: no rows are seeded — the page opens its OWN DDP subscription after hydration and
     fills in, then addTask pushes a new row back through the open subscription *)
  |> expect_text ".tasks .task-items" "Buy milk"
  |> expect_text ".tasks .task-items" "Walk the dog"
  |> click ".tasks .add-task"
  |> expect_text ".tasks .task-items" "Task 1"
  |> ignore
