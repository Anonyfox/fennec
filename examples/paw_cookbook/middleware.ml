(* The middleware battery and the two pipeline phases — the one place to reach past [Paw.endpoint]
   (which puts everything in one always-run phase) for the {!Endpoint} builder, which itself pipes.

   The ALWAYS phase ([use]) wraps every request — including 404s: logging, a correlation id,
   security headers, CORS. The MATCHED phase ([use_matched]) runs only when a route matches, so an
   unknown URL still 404s instead of being throttled or measured: rate limiting and metrics. *)

let app =
  Paw.Endpoint.make ~name:"app" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.use (Paw.Request_id.make ())
  |> Paw.Endpoint.use (Paw.Security_headers.make ())
  |> Paw.Endpoint.use
       (Paw.Cors.make ~origins:(Paw.Cors.These [ "https://app.example.com" ]) ~credentials:true ())
  |> Paw.Endpoint.use_matched (Paw.Rate_limit.make ~capacity:100 ~per_second:10. ())
  |> Paw.Endpoint.use_matched
       (Paw.Metrics.make (fun ~meth ~path ~status ~duration_ms ->
            Printf.printf "%s %s -> %d (%.1fms)\n%!" meth path status duration_ms))
  |> Paw.Endpoint.get "/" (fun c -> c |> Paw.html "<h1>home</h1>")
  |> Paw.Endpoint.get "/items/:id" (fun c -> c |> Paw.json {|{"ok":true}|})

let () = Paw.serve [ app ]
