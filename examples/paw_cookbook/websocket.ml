(* WebSockets alongside HTTP. The upgrade paw answers a path by handing you a channel; set
   [on_text] / [on_close] and call [send] from anywhere. Here, a trivial echo. *)

let realtime =
  Paw.Endpoint.make ~name:"realtime" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.html c {|<h1>connect to /ws</h1>|})
  |> Paw.Endpoint.use
       (Paw.Websocket.make "/ws" (fun ch ->
            let open Paw.Ws_channel in
            ch.on_text <- (fun msg -> ch.send ("echo: " ^ msg));
            ch.on_close <- (fun () -> print_endline "ws closed")))

let () = Paw.serve [ realtime ]
