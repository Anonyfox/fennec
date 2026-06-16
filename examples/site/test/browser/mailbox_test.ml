(* The dev mailbox, end to end in a real browser. Open /dev/mailbox (a standalone realtime SPA), trigger a
   server-side send via fetch, and watch the captured email arrive over the SAME DDP loop the tasks demo
   uses: Mail.send → set_dev_capture tee → _fennec_outbox insert → publication push → jsoo client → Fur
   signal → DOM. Then open the row and confirm the rendered headers + the sandboxed HTML-body iframe. *)
open Fennec_hunt.Live

let hydrated p = p |> wait_for ~descr:"hydration" "window.__fur_hydrated === true"

let%browser "dev mailbox: a sent email is captured and rendered live, then opens" = fun page ->
  page
  |> goto "/dev/mailbox" |> hydrated
  |> eval "fetch('/dev/send-test-mail')" (* trigger a server-side send *)
  |> expect_text ".mrow-subj" "Welcome to Fennec" (* arrives over DDP — no refresh *)
  |> click ".mrow" (* open it *)
  |> expect_text ".msg-head" "you@example.com" (* the rendered recipient header *)
  |> expect_js ~descr:"HTML body renders in a sandboxed <iframe srcdoc>"
       "!!document.querySelector('iframe.msg-html[sandbox][srcdoc]')"
  |> ignore
