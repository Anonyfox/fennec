(* Assets: classify (pure) + a real round-trip over a temp web root proving a CSS-only edit
   hot-swaps while a JS edit forces a reload, and unchanged content reports nothing. *)

module As = Fennec_dev.Assets

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let write path s = let oc = open_out_bin path in output_string oc s; close_out oc

let () =
  print_endline "Assets.classify:";
  eq "neither -> Nothing" (As.classify ~css:false ~other:false) As.Nothing;
  eq "css only -> Css_only" (As.classify ~css:true ~other:false) As.Css_only;
  eq "other -> Reload" (As.classify ~css:false ~other:true) As.Reload;
  eq "css + other -> Reload (a JS change wins)" (As.classify ~css:true ~other:true) As.Reload

let () =
  print_endline "Assets.poll (temp web root):";
  let dir = Filename.temp_file "fennec_assets" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  write (Filename.concat dir "main.css") "a{}";
  write (Filename.concat dir "main.js") "x=1";
  let a = As.create ~dir in
  As.seed a;
  eq "no change after seed -> Nothing" (As.poll a) As.Nothing;
  write (Filename.concat dir "main.css") "a{color:red}";
  eq "css edit -> Css_only" (As.poll a) As.Css_only;
  write (Filename.concat dir "main.js") "x=2";
  eq "js edit -> Reload" (As.poll a) As.Reload;
  eq "stable again -> Nothing" (As.poll a) As.Nothing;
  (* deleting a tracked asset must force a reload (and not linger as a stale hash) *)
  Sys.remove (Filename.concat dir "main.css");
  eq "deleting a tracked file -> Reload" (As.poll a) As.Reload;
  eq "deletion settled -> Nothing" (As.poll a) As.Nothing;
  List.iter (fun f -> try Sys.remove (Filename.concat dir f) with _ -> ()) [ "main.css"; "main.js" ];
  (try Unix.rmdir dir with _ -> ());
  if !fails = 0 then print_endline "all Assets tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
