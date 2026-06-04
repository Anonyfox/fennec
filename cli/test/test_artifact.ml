(* Artifact.bytecode_ready: a complete OCaml bytecode exe ends with the runtime magic; a
   half-written one (what dune leaves mid-rebuild) does not. *)

module A = Fennec_dev.Artifact

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let write path contents = let oc = open_out_bin path in output_string oc contents; close_out oc

let () =
  print_endline "Artifact.bytecode_ready:";
  let tmp = Filename.temp_file "fennec_artifact" ".bc" in
  (* a complete image: payload then the 12-byte magic trailer *)
  write tmp (String.make 100 'x' ^ "Caml1999X036");
  check "complete bytecode (magic trailer present) -> true" (A.bytecode_ready tmp);
  (* a half-written image: no magic trailer yet *)
  write tmp (String.make 100 'x');
  check "no magic trailer -> false" (not (A.bytecode_ready tmp));
  (* a truncated/tiny file *)
  write tmp "short";
  check "too short -> false" (not (A.bytecode_ready tmp));
  Sys.remove tmp;
  check "missing file -> false" (not (A.bytecode_ready "/no/such/file.bc"));
  if !fails = 0 then print_endline "all Artifact tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
