(* PROOF + regression guard for the fennec-mlx-pp DRIVER's diagnostics + stdin rewrite path.

   The driver (fennec_mlx_pp.exe) is handed to us as argv.(1) by the runtest rule (a dune dep). It
   pipes the pre-passed source to mlx-pp via stdin on the REWRITE path. Two things this pins:

     1. A HARD syntax error from mlx-pp (no AST to remap) must point at the user's ORIGINAL .mlx, not
        at `*stdin*` (the name mlx-pp uses when reading stdin) — we rewrite the filename token in
        mlx-pp's captured stderr. This is strictly better than the old temp-file design (a /tmp path).
     2. The REWRITE-path success case still emits a valid binary AST (the impl magic) on stdout, exit 0,
        with NOTHING on stderr — i.e. capturing stderr did not break the happy path.

   Needs `mlx-pp` on PATH (the driver execs it); SKIPS if absent, exactly like the sibling proofs. *)

let on_path name = Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" name) = 0

let write_file path contents =
  let oc = open_out_bin path in output_string oc contents; close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in close_in ic; s

(* run [driver file], capturing stdout to [out_path] and stderr to [err_path]; return the exit code. *)
let run_driver driver file ~out_path ~err_path =
  let cmd = Printf.sprintf "%s %s >%s 2>%s"
      (Filename.quote driver) (Filename.quote file)
      (Filename.quote out_path) (Filename.quote err_path) in
  Sys.command cmd

let contains hay needle =
  let n = String.length hay and m = String.length needle in
  let r = ref false and i = ref 0 in
  while (not !r) && !i <= n - m do (if String.sub hay !i m = needle then r := true); incr i done;
  !r

let fail = ref 0
let check name cond = if cond then Printf.printf "ok:   %s\n" name
                      else (incr fail; Printf.printf "FAIL: %s\n" name)

let () =
  let driver = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "" in
  if driver = "" || not (Sys.file_exists driver) then
    (print_endline "driver_diagnostics: no driver exe argument — SKIPPED"; exit 0);
  if not (on_path "mlx-pp") then
    (print_endline "driver_diagnostics: mlx-pp not on PATH — SKIPPED"; exit 0);

  let dir = Filename.get_temp_dir_name () in
  let out_path = Filename.concat dir "fennec_drv_out.bin" in
  let err_path = Filename.concat dir "fennec_drv_err.txt" in

  (* (1) SYNTAX ERROR on the rewrite path → stderr must name the real file, never *stdin*. The bare
     text `oops here` forces the rewrite path; the dangling `{1 +` is the syntax error. *)
  let bad = Filename.concat dir "fennec_drv_syntax_err.mlx" in
  write_file bad "let v = <p>oops here {1 +";
  let code = run_driver driver bad ~out_path ~err_path in
  let err = read_file err_path in
  check "syntax error exits non-zero" (code <> 0);
  check "syntax-error stderr names the ORIGINAL .mlx path" (contains err bad);
  check "syntax-error stderr does NOT leak *stdin*" (not (contains err "*stdin*"));

  (* (2) REWRITE-path SUCCESS → valid AST magic on stdout, clean exit, empty stderr. *)
  let good = Filename.concat dir "fennec_drv_ok.mlx" in
  write_file good "let v = <p>hello world {1 + 2}</p>";
  let code2 = run_driver driver good ~out_path ~err_path in
  let out2 = read_file out_path and err2 = read_file err_path in
  check "rewrite-path success exits 0" (code2 = 0);
  check "rewrite-path success emits the impl-AST magic on stdout"
    (String.length out2 >= 9 && String.sub out2 0 9 = "Caml1999M");
  check "rewrite-path success writes nothing to stderr" (String.length err2 = 0);

  List.iter (fun p -> try Sys.remove p with _ -> ()) [bad; good; out_path; err_path];
  Printf.printf "driver_diagnostics: %s\n" (if !fail = 0 then "ALL CHECKS PASSED" else "FAILURES");
  if !fail > 0 then exit 1
