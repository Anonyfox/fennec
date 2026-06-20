(* Version 1 of the reload fixture. Top-level runs at Dynlink-load time (like an inline-test runner),
   prints a version banner using the cross-file marker, and exits 0 (a "passing test"). *)
let () = Printf.printf "ok WW widget %s v1\n%!" Ww_marker.tag
