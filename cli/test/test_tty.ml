(* Tty: the pure escape-builders + visible-width measurement (capability detection is IO and
   covered indirectly by the live integration). *)

module T = Fennec_dev.Tty

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let caps ~color ~hyperlinks = { T.color; hyperlinks; interactive = true; width = 80 }

let () =
  print_endline "Tty.sgr / hyperlink / visible_width:";
  (* plain caps: identity *)
  check "sgr is identity without colour" (T.sgr T.plain "32" "x" = "x");
  check "hyperlink falls back to plain text" (T.hyperlink T.plain ~url:"http://x" ~text:"X" = "X");
  (* capable caps *)
  let c = caps ~color:true ~hyperlinks:true in
  check "sgr wraps in an SGR span" (T.sgr c "32" "x" = "\027[32mx\027[0m");
  check "hyperlink emits OSC 8" (T.hyperlink c ~url:"http://x" ~text:"X" = "\027]8;;http://x\027\\X\027]8;;\027\\");
  (* visible_width ignores escapes, counts a UTF-8 codepoint as 1 *)
  check "plain width" (T.visible_width "hello" = 5);
  check "SGR is invisible" (T.visible_width "\027[32mhi\027[0m" = 2);
  check "OSC 8 is invisible" (T.visible_width "\027]8;;u\027\\link\027]8;;\027\\" = 4);
  check "emoji counts as one column" (T.visible_width "🦊x" = 2);
  if !fails = 0 then print_endline "all Tty tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
