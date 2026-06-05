(* Ui: snapshot the rendered output against an injected buffer with fixed capabilities, so the
   renderer is deterministic. Plain caps for content; an interactive caps to prove the live region
   repaints (emits cursor-up + erase) on a fix. *)

module U = Fennec_dev.Ui
module T = Fennec_dev.Tty

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let sample =
  "File \"index.mlx\", line 11, characters 8-16:\n11 |     <h1>{greeting}</h1>\n         ^^^^^^^^\nError: Unbound value greeting\n"

let () =
  print_endline "Ui (plain mode — content):";
  let buf = Buffer.create 512 in
  let out s = Buffer.add_string buf s in
  let take () = let s = Buffer.contents buf in Buffer.clear buf; s in
  let ui = U.create ~out ~caps:T.plain () in
  U.start ui ~dir:"examples/site";
  check "banner shows the fox + name" (contains (take ()) "🦊 fennec dev");
  U.ready ui ~urls:[ "http://localhost:8200" ] ~ms:(Some 412.);
  let s = take () in
  check "ready shows the URL" (contains s "http://localhost:8200");
  check "ready shows the time" (contains s "ready in 412ms");
  check "ready shows the watched dir" (contains s "watching examples/site");
  U.ready ui ~urls:[ "http://localhost:8200" ] ~ms:(Some 99.);
  check "ready is idempotent (a second report prints nothing)" (take () = "");
  U.rebuilt ui ~trigger:[ "index.mlx changed" ] ~ms:(Some 38.);
  let s = take () in
  check "rebuilt shows file, ms, effect" (contains s "index.mlx" && contains s "38ms" && contains s "reload");
  U.restyled ui ~trigger:[ "main.scss changed" ] ~ms:(Some 9.);
  check "restyled is labelled css" (contains (take ()) "css");
  U.failed ui ~raw:sample ~trigger:[] ~serving:true;
  let s = take () in
  check "failed shows the error count" (contains s "1 error");
  check "failed notes the last good server" (contains s "last good build still serving");
  check "failed shows the location" (contains s "index.mlx:11");
  check "failed shows the message" (contains s "Unbound value greeting");
  (* THE regression: a green no-op build (revert to identical bytes) must clear the stuck panel *)
  U.resolved ui ~ms:(Some 12.);
  check "resolved clears the panel with a confirmation line" (contains (take ()) "resolved");
  U.resolved ui ~ms:(Some 5.);
  check "resolved is silent when nothing is outstanding" (take () = "");

  (* a SYNTAX error (no dune excerpt) — the UI reads the source itself for a context frame *)
  let tmp = Filename.temp_file "fennec_cf" ".ml" in
  (let oc = open_out tmp in output_string oc "let a = 1\nlet b = (\nlet c = 3\nlet d = 4\n"; close_out oc);
  U.failed ui ~raw:(Printf.sprintf "File %S, line 2, characters 8-9:\nError: Syntax error\n" tmp) ~trigger:[] ~serving:false;
  let s = take () in
  check "code frame shows the error line read from source" (contains s "let b = (");
  check "code frame includes a line of context" (contains s "let a = 1");
  check "code frame draws a caret" (contains s "^");
  U.resolved ui ~ms:None;
  (try Sys.remove tmp with _ -> ());
  (* a failed FIRST build (no last-good server) must say the server isn't running *)
  U.failed ui ~raw:sample ~trigger:[] ~serving:false;
  check "with no server the panel says 'server not running'" (contains (take ()) "server not running");
  U.resolved ui ~ms:None;

  print_endline "Ui (interactive — live region):";
  let ibuf = Buffer.create 512 in
  let iout s = Buffer.add_string ibuf s in
  let itake () = let s = Buffer.contents ibuf in Buffer.clear ibuf; s in
  let icaps = { T.color = false; hyperlinks = false; interactive = true; width = 80 } in
  let iui = U.create ~out:iout ~caps:icaps () in
  U.failed iui ~raw:sample ~trigger:[] ~serving:false;
  ignore (itake ());
  U.rebuilt iui ~trigger:[ "index.mlx changed" ] ~ms:(Some 40.);
  let s = itake () in
  check "fixing erases the region (cursor-up + erase-below)" (contains s "\027[" && contains s "\027[J");
  check "fixing then shows the success line" (contains s "reload");

  if !fails = 0 then print_endline "all Ui tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
