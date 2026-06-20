(* PROOF + regression guard for `fennec mlx-pp` — the IN-PROCESS .mlx dialect preprocessor.

   The `fennec` binary is handed to us as argv.(1) by the runtest rule (a dune dep). We run
   `fennec mlx-pp <file>` and pin two things the dialect relies on:

     1. A syntax error must point at the user's ORIGINAL .mlx (line/col), exit non-zero, and print the
        OCaml-standard `File "…", line L, …` report — NEVER a `*stdin*` or /tmp path (the in-process
        path has no spawn / stdin, and remaps the parser's location through the pre-pass posmap so the
        report is even column-/line-exact past a collapsed prose run).
     2. The success case emits a valid binary AST (the impl magic) on stdout, exit 0, with NOTHING on
        stderr — i.e. the dialect contract dune consumes is intact.

   Self-contained: it runs the real `fennec` binary (no external `mlx-pp` — the parser is vendored), so
   unlike the old driver proof it never SKIPs for a missing toolchain; it only SKIPs if argv.(1) is
   absent. *)

let write_file path contents =
  let oc = open_out_bin path in output_string oc contents; close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in close_in ic; s

(* run [fennec mlx-pp file], capturing stdout to [out_path] and stderr to [err_path]; return exit. *)
let run_pp fennec file ~out_path ~err_path =
  let cmd = Printf.sprintf "%s mlx-pp %s >%s 2>%s"
      (Filename.quote fennec) (Filename.quote file)
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
  let fennec = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "" in
  if fennec = "" || not (Sys.file_exists fennec) then
    (print_endline "driver_diagnostics: no fennec exe argument — SKIPPED"; exit 0);

  let dir = Filename.get_temp_dir_name () in
  let out_path = Filename.concat dir "fennec_pp_out.bin" in
  let err_path = Filename.concat dir "fennec_pp_err.txt" in

  (* (1) SYNTAX ERROR after a bare-text run → stderr must name the real file, never *stdin* / tmp. The
     bare text `oops here` exercises the rewrite path; the dangling `{1 +` is the syntax error. *)
  let bad = Filename.concat dir "fennec_pp_syntax_err.mlx" in
  write_file bad "let v = <p>oops here {1 +";
  let code = run_pp fennec bad ~out_path ~err_path in
  let err = read_file err_path in
  check "syntax error exits non-zero" (code <> 0);
  check "syntax-error stderr names the ORIGINAL .mlx path" (contains err bad);
  check "syntax-error stderr does NOT leak *stdin*" (not (contains err "*stdin*"));
  check "syntax-error stderr is the OCaml-standard File/line report" (contains err "File " && contains err "line ");

  (* (2) SUCCESS → valid AST magic on stdout, clean exit, empty stderr. *)
  let good = Filename.concat dir "fennec_pp_ok.mlx" in
  write_file good "let v = <p>hello world {1 + 2}</p>";
  let code2 = run_pp fennec good ~out_path ~err_path in
  let out2 = read_file out_path and err2 = read_file err_path in
  check "success exits 0" (code2 = 0);
  check "success emits the impl-AST magic on stdout"
    (String.length out2 >= 9 && String.sub out2 0 9 = "Caml1999M");
  check "success writes nothing to stderr" (String.length err2 = 0);

  List.iter (fun p -> try Sys.remove p with _ -> ()) [bad; good; out_path; err_path];
  Printf.printf "driver_diagnostics: %s\n" (if !fail = 0 then "ALL CHECKS PASSED" else "FAILURES");
  if !fail > 0 then exit 1
