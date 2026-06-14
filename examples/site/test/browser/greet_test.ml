(* The /greet HANDLER end-to-end in a real browser — a standalone SPA with its OWN isolated bundle,
   mounted manually. SSR shows the cross-stage payload pre-JS; the handler's own bundle hydrates it
   into live state (the Counter starts at the server-computed count); it is reused at a path-param
   route; and live data reconstructs over DDP. *)
open Fennec_hunt.Live

let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let%browser "handler /greet: server-seeded payload SSRs, then its own bundle hydrates into live state" =
 fun page ->
  page
  |> goto "/greet?name=Ada"
  |> expect_text "#greet h1" "Hello, Ada!"
  |> expect_text "#greet .seed" "server-computed start = 3"
  |> hydrated
  |> expect_text "#greet .count" "3" (* the live counter STARTS at the seeded count *)
  |> click ".cbtn.inc"
  |> expect_text "#greet .count" "4"
  |> ignore

let%browser "handler reused at a path-param route /hi/:name" = fun page ->
  page
  |> goto "/hi/World"
  |> expect_text "#greet h1" "Hello, World!"
  |> hydrated
  |> expect_text "#greet .count" "5" (* String.length "World" = 5, seeded into the live counter *)
  |> ignore

let%browser "handler: live data RECONSTRUCTS via Pulse after hydration (seed static, reconstruct live)" =
 fun page ->
  page
  |> goto "/greet?name=Ada"
  |> hydrated
  |> expect_text ".tasks .task-items" "Buy milk"
  |> click ".tasks .add-task"
  |> expect_text ".tasks .task-items" "Task 1"
  |> ignore
