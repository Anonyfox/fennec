(* Host_trie: the reversed-label trie that makes host matching O(1) exact / O(depth) suffix.
   Every case the router relies on must produce identical results here — the trie is a
   performance optimization, not a behavior change. *)

module T = Fennec_server.Host_trie
module P = Fennec_server.Host_pattern

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let pat s = Result.get_ok (P.of_string s)

let () =
  print_endline "Host_trie (exact matching):";
  let t = T.build [ (pat "acme.com", "acme"); (pat "admin.acme.com", "admin") ] in
  check "exact match" (T.lookup t ~host:"acme.com" = Some "acme");
  check "exact match (subdomain)" (T.lookup t ~host:"admin.acme.com" = Some "admin");
  check "exact miss" (T.lookup t ~host:"other.com" = None);
  check "partial match (prefix only) is not a hit" (T.lookup t ~host:"x.admin.acme.com" = None);
  check "case-insensitive" (T.lookup t ~host:"ACME.COM" = Some "acme");
  check "host with :port" (T.lookup t ~host:"acme.com:4000" = Some "acme");
  check "empty host" (T.lookup t ~host:"" = None);

  print_endline "Host_trie (wildcard / suffix matching):";
  let tw = T.build [ (pat "*.acme.com", "wild") ] in
  check "wildcard matches a subdomain" (T.lookup tw ~host:"api.acme.com" = Some "wild");
  check "wildcard matches deep subdomain" (T.lookup tw ~host:"a.b.acme.com" = Some "wild");
  check "wildcard needs >=1 label (base alone fails)" (T.lookup tw ~host:"acme.com" = None);
  check "wildcard rejects other base" (T.lookup tw ~host:"api.other.com" = None);

  print_endline "Host_trie (precedence):";
  (* exact beats wildcard at the same level *)
  let tp = T.build [ (pat "admin.acme.com", "exact"); (pat "*.acme.com", "wild") ] in
  check "exact beats wildcard" (T.lookup tp ~host:"admin.acme.com" = Some "exact");
  check "non-exact falls to wildcard" (T.lookup tp ~host:"api.acme.com" = Some "wild");
  (* deeper wildcard beats shallower wildcard *)
  let td = T.build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  check "deeper wildcard wins (x.api.acme.com)" (T.lookup td ~host:"x.api.acme.com" = Some "deep");
  check "shallower wildcard still catches its level" (T.lookup td ~host:"x.acme.com" = Some "shallow");
  check "deep base alone (api.acme.com) only matches shallow (not deep — needs >=1 label)" (T.lookup td ~host:"api.acme.com" = Some "shallow");

  print_endline "Host_trie (mixed exact + wildcards):";
  let tm = T.build [
    (pat "acme.com", "root");
    (pat "admin.acme.com", "admin");
    (pat "*.acme.com", "catch");
    (pat "*.api.acme.com", "api_catch");
  ] in
  check "root exact" (T.lookup tm ~host:"acme.com" = Some "root");
  check "admin exact" (T.lookup tm ~host:"admin.acme.com" = Some "admin");
  check "random subdomain -> catch" (T.lookup tm ~host:"random.acme.com" = Some "catch");
  check "api subdomain -> api_catch (deeper)" (T.lookup tm ~host:"x.api.acme.com" = Some "api_catch");
  check "api base -> catch (not api_catch)" (T.lookup tm ~host:"api.acme.com" = Some "catch");
  check "completely unrelated -> None" (T.lookup tm ~host:"example.org" = None);

  print_endline "Host_trie (edge cases):";
  let te = T.build [] in
  check "empty trie -> None for any host" (T.lookup te ~host:"anything.com" = None);
  let ts = T.build [ (pat "a.com", "a") ] in
  check "single-label TLD mismatch" (T.lookup ts ~host:"b.com" = None);
  check "trailing dot in host" (T.lookup ts ~host:"a.com." = Some "a");

  if !fails = 0 then print_endline "all Host_trie tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
