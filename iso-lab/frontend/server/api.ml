(* The app's backend: in-process data sources (path -> json). Called directly on the
   server — no HTTP-to-self. Later: Mongo publishers live here too. SERVER-ONLY. *)
let source = function "/api/greeting" -> Some "Hello from the server \xf0\x9f\x91\x8b" | _ -> None
