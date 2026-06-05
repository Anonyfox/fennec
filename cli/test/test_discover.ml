(* Discover.calls_serve is the heuristic that identifies THE server (the one executable calling
   Fennec.serve). It gates a destructive-ish decision (pick the wrong one and you supervise the
   wrong exe), and it ran with zero coverage. Pin both what it accepts and — to avoid false
   positives — what it must reject (the word in prose, in a string, or as part of another ident). *)

module D = Fennec_dev.Discover

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  print_endline "Discover.calls_serve:";
  (* accepts: real call shapes *)
  check "qualified Fennec.serve" (D.calls_serve "let () = Fennec.serve [ ep ]");
  check "open Fennec then bare serve" (D.calls_serve "open Fennec\nlet () = serve ~port:8200 [ ep ]");
  check "open Fennec.App then serve" (D.calls_serve "open Fennec.App\nlet () = serve [ ep ]");
  (* rejects: not a serve call *)
  check "preserve is not serve" (not (D.calls_serve "let xs = preserve_order items"));
  check "self_serve is not serve" (not (D.calls_serve "open Fennec\nlet self_serve = 1"));
  check "serve in a comment is not a call" (not (D.calls_serve "open Fennec\n(* remember to serve *)\nlet () = ()"));
  check "serve in a string is not a call" (not (D.calls_serve "open Fennec\nlet s = \"please serve\""));
  check "serve in a {|raw string|} is not a call" (not (D.calls_serve "open Fennec\nlet s = {|serve|}"));
  check "merely linking Fennec is not starting it" (not (D.calls_serve "open Fennec\nlet app = Endpoint.get \"/\" h"));
  check "no mention of serve at all" (not (D.calls_serve "let () = print_endline \"hi\""));
  if !fails = 0 then print_endline "all Discover tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
