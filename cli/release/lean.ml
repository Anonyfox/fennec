(* See lean.mli. The prod-lean scan.

   A built native binary embeds the names of the modules it links (verified: an e2e-linked binary
   contains "Fennec_hunt"/"Yojson" ~18×; a clean server.exe contains them 0×). So we read the binary
   and assert none of the forbidden module-name needles appear. Pure Stdlib — no platform tooling. *)

let forbidden = [ "Fennec_hunt__Cdp"; "Fennec_hunt__Chrome"; "Fennec_hunt__Http_client"; "Yojson" ]

type verdict =
  | Clean
  | Leaked of string list

(* [Fennec_hunt_unit.str_contains] is the allocation-free substring search (it does not cut a
   [String.sub] per position), so it stays cheap scanning the multi-MB binary here. *)
let needles_in (bytes : string) : verdict =
  match List.filter (Fennec_hunt_unit.str_contains bytes) forbidden with [] -> Clean | leaked -> Leaked leaked

let scan ~exe : (verdict, string) result =
  match Util.read_file exe with bytes -> Ok (needles_in bytes) | exception Sys_error msg -> Error msg

(* ──── tests ──── *)

let%test "a binary embedding the CDP machinery leaks it" =
  needles_in "....Fennec_hunt__Cdp_session...." = Leaked [ "Fennec_hunt__Cdp" ]

let%test "yojson is forbidden (the heavy transitive JSON dep)" = needles_in "x Yojson.Safe y" = Leaked [ "Yojson" ]

let%test "the 1KB inline-test runtime alone is clean (single-underscore, not a needle)" =
  needles_in "Fennec_hunt_unit.run was registered" = Clean

let%test "a typical clean server image is clean" = needles_in "Paw Fennec Fur Bson Eio Pulse" = Clean

let%test "every leak is reported, in list order" =
  needles_in "has Fennec_hunt__Chrome and also Yojson here" = Leaked [ "Fennec_hunt__Chrome"; "Yojson" ]
