(* Server-Sent Events — push live updates to the browser over one long-lived response (its
   [EventSource]), no websocket needed. The handler [push]es events as they occur; here a short
   progress stream. A real app blocks on a queue / pub-sub between pushes. Open http://localhost:4000.
   For comment heartbeats and full control, reach for [Paw.Sse.stream]. *)

let page =
  {|<!doctype html><meta charset=utf-8><title>SSE</title>
<h1>progress</h1><pre id=log></pre>
<script>
  const es = new EventSource('/events')
  es.addEventListener('step', e => { log.textContent += e.data + '\n' })
  es.addEventListener('done', () => es.close())
</script>|}

let app =
  Paw.endpoint ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.get "/" (fun c -> c |> Paw.html page)
  |> Paw.get "/events" (fun c ->
         c
         |> Paw.sse (fun ~push ->
                for step = 1 to 5 do
                  push ~event:"step" (Printf.sprintf "%d/5" step)
                done;
                push ~event:"done" ""))

let () = Paw.serve [ app ]
