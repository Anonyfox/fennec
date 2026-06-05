(* The CLI<->server dev wire (Fennec_core.Dev_proto). The line (de)serializers must round-trip, and
   each parser must REJECT the other line + arbitrary text — so a foreign log line is never mistaken
   for a URL report or a port complaint. If a prefix/format/code drifts, these fail loudly here
   instead of the dev URL/"ready" banner silently never appearing. *)

module P = Fennec_core.Dev_proto

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  print_endline "Dev_proto:";
  (* urls: round-trip incl. the empty list and a single url *)
  check "urls round-trips" (P.parse_urls_line (P.urls_line [ "http://localhost:8200"; "http://localhost:8201" ]) = Some [ "http://localhost:8200"; "http://localhost:8201" ]);
  check "urls round-trips (single)" (P.parse_urls_line (P.urls_line [ "http://localhost:8200" ]) = Some [ "http://localhost:8200" ]);
  check "urls round-trips (empty)" (P.parse_urls_line (P.urls_line []) = Some []);
  check "urls line carries the prefix" (P.starts_with (P.urls_line [ "x" ]) P.urls_prefix);
  (* port: round-trip across the range *)
  check "port round-trips (8200)" (P.parse_port_busy (P.port_busy_line 8200) = Some 8200);
  check "port round-trips (1)" (P.parse_port_busy (P.port_busy_line 1) = Some 1);
  check "port round-trips (65535)" (P.parse_port_busy (P.port_busy_line 65535) = Some 65535);
  (* the distinct port-conflict exit code is the documented 98 (both sides hard-depend on it) *)
  check "port_in_use_exit is 98" (P.port_in_use_exit = 98);
  (* parsers reject the OTHER line + arbitrary text, so classification can't cross the wires *)
  check "parse_urls rejects a port line" (P.parse_urls_line (P.port_busy_line 8200) = None);
  check "parse_port rejects a urls line" (P.parse_port_busy (P.urls_line [ "http://x" ]) = None);
  check "parse_urls rejects chatter" (P.parse_urls_line "[fennec] serving 2 endpoint(s)" = None);
  check "parse_urls rejects an app log" (P.parse_urls_line "hello from the app" = None);
  check "parse_port rejects an app log" (P.parse_port_busy "listening on something" = None);
  (* chatter must be distinguishable from a urls line (both begin "[fennec") *)
  check "a urls line is NOT chatter" (not (P.starts_with (P.urls_line [ "x" ]) P.chatter_prefix));
  check "chatter IS chatter" (P.starts_with "[fennec] serving" P.chatter_prefix);
  if !fails = 0 then print_endline "all Dev_proto tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
