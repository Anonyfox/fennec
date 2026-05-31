(* Exercises the full native pipeline: esbuild bundling (cold + warm rebuild +
   build error) and the Lightning CSS / grass engine. *)

let tmp = Filename.get_temp_dir_name ()

let write name contents =
  let path = Filename.concat tmp name in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (Printf.printf "  FAIL %s\n" name; exit 1)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let () =
  print_endline "esbuild:";
  let _ = write "fk_dep.js" "export const greet = (n) => `hi ${n}`;\n" in
  let entry =
    write "fk_app.js"
      "import { greet } from './fk_dep.js';\nwindow.msg = greet('fennec');\n"
  in

  (* one-shot bundle: import is resolved & inlined *)
  let js = Fennec_buildkit.Esbuild.build ~entry () in
  check "bundles and resolves imports" (contains js "hi ");
  check "produces non-empty output" (String.length js > 0);

  (* minified bundle is smaller *)
  let min = Fennec_buildkit.Esbuild.build ~entry ~minify:true () in
  check "minify shrinks output" (String.length min < String.length js);

  (* warm context: repeated rebuilds are stable *)
  let ctx = Fennec_buildkit.Esbuild.create ~entry () in
  let a = Fennec_buildkit.Esbuild.rebuild ctx in
  let b = Fennec_buildkit.Esbuild.rebuild ctx in
  check "warm rebuild is deterministic" (a = b);
  Fennec_buildkit.Esbuild.dispose ctx;

  (* build error surfaces as Failure *)
  let bad = write "fk_bad.js" "import { x } from './does_not_exist.js';\n" in
  check "build error raises Failure"
    (try ignore (Fennec_buildkit.Esbuild.build ~entry:bad ()); false
     with Failure _ -> true);

  print_endline "css:";
  let css = Fennec_buildkit.Css.transform ~minify:true ".a { color: #ffffff; }" in
  check "minifies css" (contains css "#fff" || contains css "white");

  let scss =
    Fennec_buildkit.Css.scss ~minify:true
      "$c: red;\n.btn { color: $c; &:hover { color: darken($c, 10%); } }"
  in
  check "compiles scss nesting + vars" (contains scss ".btn" && contains scss ":hover");

  print_endline "all buildkit tests passed."
