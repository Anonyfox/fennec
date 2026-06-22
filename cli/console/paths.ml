(* See paths.mli. A short, project-stable socket name under the temp dir. *)

let socket ~root =
  let h = Digest.to_hex (Digest.string root) in
  Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "fennec-con-%s.sock" (String.sub h 0 16))
