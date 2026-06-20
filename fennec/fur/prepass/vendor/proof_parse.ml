(* Vendor smoke proof: parse a JSX-bearing buffer through the vendored parser and run it through
   the same ppxlib Convert mlx-pp's pp.ml uses, proving the marshalled-AST pipeline works. *)
let () =
  let lb = Lexing.from_string {|let v = <div className="x">(greeting)</div>|} in
  Lexing.set_filename lb "buf.mlx";
  let str = Fennec_mlx.Parse.implementation lb in
  let module Conv =
    Ppxlib_ast.Convert (Ppxlib_ast__Versions.OCaml_501) (Ppxlib_ast.Compiler_version)
  in
  let _ = Conv.copy_structure str in
  Printf.printf "vendored parser OK: %d structure item(s)\n" (List.length str)
