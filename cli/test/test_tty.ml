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
  (* truncated escapes must terminate and count the visible prefix (the scan-loop boundaries) *)
  check "a truncated CSI (ESC[ at end) terminates, counts prefix" (T.visible_width "ab\027[" = 2);
  check "a truncated OSC (no ST) terminates, counts prefix" (T.visible_width "ab\027]8;;u" = 2);
  (* documented limitation: codepoints, not graphemes — a ZWJ emoji counts as its codepoints (>1) *)
  check "a ZWJ grapheme counts by codepoints (documented, not 1)" (T.visible_width "👩‍🚀" > 1);

  print_endline "Tty.detect / supports_hyperlinks (env-driven):";
  (* colour precedence NO_COLOR > FORCE_COLOR > tty — the first two are deterministic regardless of
     whether stdout is a tty (so this is stable under `dune runtest`, which pipes stdout) *)
  Unix.putenv "NO_COLOR" "";
  Unix.putenv "FORCE_COLOR" "1";
  check "FORCE_COLOR forces colour on" (T.detect ()).T.color;
  Unix.putenv "NO_COLOR" "1";
  check "NO_COLOR overrides FORCE_COLOR" (not (T.detect ()).T.color);
  Unix.putenv "NO_COLOR" "";
  Unix.putenv "FORCE_COLOR" "";
  (* hyperlink allow-list *)
  List.iter (fun k -> Unix.putenv k "") [ "CI"; "WT_SESSION"; "KITTY_WINDOW_ID"; "VTE_VERSION" ];
  Unix.putenv "TERM_PROGRAM" "iTerm.app";
  check "iTerm2 supports OSC 8" (T.supports_hyperlinks ());
  Unix.putenv "TERM_PROGRAM" "Apple_Terminal";
  check "Terminal.app does NOT (no OSC 8)" (not (T.supports_hyperlinks ()));
  Unix.putenv "TERM_PROGRAM" "iTerm.app";
  Unix.putenv "CI" "true";
  check "CI disables hyperlinks (overrides the terminal)" (not (T.supports_hyperlinks ()));
  Unix.putenv "CI" "";
  Unix.putenv "TERM_PROGRAM" "";
  Unix.putenv "WT_SESSION" "1";
  check "Windows Terminal (WT_SESSION) supports OSC 8" (T.supports_hyperlinks ());
  Unix.putenv "WT_SESSION" "";
  if !fails = 0 then print_endline "all Tty tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
