(* A tiny Eio HTTP/1.1 server for the Http-layer I/O e2e proofs — pure Eio, no framework.
   Routes (matched on the request line):
     GET /ok        → 200 "ok"
     GET /slow      → sleeps 5s, then 200 (used to prove per-request timeout)
     GET /redirect  → 302, Location: /ok  (used to prove redirect following)
     GET /chunked   → 200, chunked transfer-encoding "ab"+"cd"+"ef"
     *              → 404
   Listens on :4555. Spawned by probe_check.ml via `hunt ~spawn`. *)

let respond ?(status = "200 OK") ?(headers = "") body =
  Printf.sprintf "HTTP/1.1 %s\r\nContent-Length: %d\r\nConnection: close\r\n%s\r\n%s"
    status (String.length body) headers body

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let flaky_hits = ref 0 in (* /flaky returns 503 for the first 2 requests, then 200 *)
  let sock = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 4555)) in
  Eio.Net.run_server sock ~on_error:(fun _ -> ()) (fun flow _addr ->
      let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
      let line = try Eio.Buf_read.line r with _ -> "" in
      let path = match String.split_on_char ' ' line with _ :: p :: _ -> p | _ -> "/" in
      let reply =
        match path with
        | "/ok" -> respond "ok"
        | "/slow" -> Eio.Time.sleep clock 5.0; respond "slow"
        | "/redirect" -> respond ~status:"302 Found" ~headers:"Location: /ok\r\n" ""
        | "/chunked" ->
          "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n\
           2\r\nab\r\n2\r\ncd\r\n2\r\nef\r\n0\r\n\r\n"
        | "/flaky" ->
          incr flaky_hits;
          if !flaky_hits >= 3 then respond {|{"state":"done"}|}
          else respond ~status:"503 Service Unavailable" {|{"state":"pending"}|}
        | _ -> respond ~status:"404 Not Found" "nope"
      in
      try Eio.Flow.copy_string reply flow with _ -> ())
