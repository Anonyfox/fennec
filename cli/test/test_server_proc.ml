(* Server_proc.classify_line is the pure routing decision for every line the server prints — the
   one piece of logic in the otherwise-IO server child that's worth pinning. (The wire parsing it
   composes is covered by test_dev_proto; here we pin the 4-way classification + the fallthroughs.) *)

module S = Fennec_dev.Server_proc

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  print_endline "Server_proc.classify_line:";
  check "a urls report -> Urls" (S.classify_line "[fennec:urls] http://localhost:8200 http://localhost:8201" = S.Urls [ "http://localhost:8200"; "http://localhost:8201" ]);
  check "a port-busy line -> Port_busy" (S.classify_line "fennec: port 8200 is already in use — another server is holding it." = S.Port_busy 8200);
  check "framework chatter -> Chatter" (S.classify_line "[fennec] serving 2 endpoint(s)" = S.Chatter);
  check "a blank line -> Chatter" (S.classify_line "" = S.Chatter);
  check "whitespace-only -> Chatter" (S.classify_line "   " = S.Chatter);
  check "an app log -> App_log (trimmed)" (S.classify_line "  hello from the app  " = S.App_log "hello from the app");
  check "leading/trailing space on a urls line still parses" (S.classify_line "  [fennec:urls] http://x  " = S.Urls [ "http://x" ]);
  if !fails = 0 then print_endline "all Server_proc tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
