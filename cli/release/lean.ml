(* See lean.mli. The prod-lean scan.

   A built native binary embeds the names of the modules it links (verified: an e2e-linked binary
   contains "Fennec_hunt"/"Yojson" ~18×; a clean server.exe contains them 0×). So we read the binary
   and assert none of the forbidden module-name needles appear. Pure Stdlib — no platform tooling. *)

let forbidden = [ "Fennec_hunt__Cdp"; "Fennec_hunt__Chrome"; "Fennec_hunt__Http_client"; "Yojson" ]

type verdict =
  | Clean
  | Leaked of string list

(* allocation-free substring test (the haystack is a multi-MB binary) *)
let contains hay ndl =
  let hl = String.length hay and nl = String.length ndl in
  if nl = 0 then true
  else begin
    let rec matches i j = j = nl || (hay.[i + j] = ndl.[j] && matches i (j + 1)) in
    let rec scan i = i + nl <= hl && (matches i 0 || scan (i + 1)) in
    scan 0
  end

let needles_in (bytes : string) : verdict =
  match List.filter (fun ndl -> contains bytes ndl) forbidden with [] -> Clean | leaked -> Leaked leaked

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let scan ~exe : (verdict, string) result =
  match read_file exe with bytes -> Ok (needles_in bytes) | exception Sys_error msg -> Error msg

(* ──── tests ──── *)

let%test "a binary embedding the CDP machinery leaks it" =
  needles_in "....Fennec_hunt__Cdp_session...." = Leaked [ "Fennec_hunt__Cdp" ]

let%test "yojson is forbidden (the heavy transitive JSON dep)" = needles_in "x Yojson.Safe y" = Leaked [ "Yojson" ]

let%test "the 1KB inline-test runtime alone is clean (single-underscore, not a needle)" =
  needles_in "Fennec_hunt_unit.run was registered" = Clean

let%test "a typical clean server image is clean" = needles_in "Paw Fennec Fur Bson Eio Pulse" = Clean

let%test "every leak is reported, in list order" =
  needles_in "has Fennec_hunt__Chrome and also Yojson here" = Leaked [ "Fennec_hunt__Chrome"; "Yojson" ]
