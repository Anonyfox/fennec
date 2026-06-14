(* The /greet PAGE end-to-end in a real browser — a standalone single-page SPA with its OWN isolated
   jsoo bundle (no router). Proves the cross-stage boundary: the server computes the payload, seeds
   it, and the page SSRs it (the seeded text is present pre-JS) and then hydrates into something
   ALIVE (its own bundle boots and the <Counter/> increments). Same SSR + hydration machinery an app
   uses — just one view, no client router. *)
open Fennec_hunt.Live

let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let%browser "page: server-seeded payload SSRs, then the page's own bundle hydrates + goes interactive" =
 fun page ->
  page
  |> goto "/greet?name=Ada"
  |> expect_text "#greet h1" "Hello, Ada!" (* the cross-stage payload, server-computed, present pre-JS *)
  |> expect_text "#greet .seed" "server-computed start = 3" (* count = String.length "Ada" *)
  |> hydrated (* the page's OWN bundle boots — no app router involved *)
  |> click ".cbtn.inc"
  |> expect_text ".count" "1" (* alive: local signal state after hydration *)
  |> click ".cbtn.inc"
  |> expect_text ".count" "2"
  |> click ".cbtn.dec"
  |> expect_text ".count" "1"
  |> ignore

let%browser "page: default payload when no query param" = fun page ->
  page
  |> goto "/greet"
  |> expect_text "#greet h1" "Hello, world!"
  |> hydrated
  |> ignore
