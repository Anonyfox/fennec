(* Workflow reactions over the in-memory backend: after fires post-commit with the result; before
   guards veto (roll back) and suppress after; a body raise rolls back; nested workflows flatten so a
   nested after fires only at the OUTERMOST commit, and an outer rollback suppresses the inner one. *)

module W = Fennec_pulse_workflow.Workflow
module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let count c = List.length (D.find c (Fennec_mongo_backend.query ()))
let person n = B.doc [ ("name", B.str n) ]

exception Boom

let () =
  (* 1. after fires post-commit with the workflow's result *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let place = W.make "place" (fun name -> ignore (D.insert c (person name)); String.uppercase_ascii name) in
  W.after place (fun r -> log := r :: !log);
  let r = W.call place "ada" in
  check "workflow returns result" (r = "ADA");
  check "workflow body committed" (count c = 1);
  check "after fired with result" (!log = [ "ADA" ]);

  (* 2. a before guard vetoes: it rolls back the body and suppresses after; a later good call works *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let w = W.make "w" (fun name -> ignore (D.insert c (person name)); name) in
  W.after w (fun r -> log := r :: !log);
  W.before w (fun name -> if name = "bad" then raise Boom);
  (try ignore (W.call w "bad") with Boom -> ());
  check "before veto rolls back the body" (count c = 0);
  check "before veto suppresses after" (!log = []);
  ignore (W.call w "ok");
  check "a good call after a veto commits + fires after" (count c = 1 && !log = [ "ok" ]);

  (* 3. a raise in the body rolls back and fires no after *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let w = W.make "w" (fun name -> ignore (D.insert c (person name)); raise Boom) in
  W.after w (fun _ -> log := "after" :: !log);
  (try ignore (W.call w "x") with Boom -> ());
  check "body raise rolls back" (count c = 0);
  check "body raise suppresses after" (!log = []);

  (* 4. nested rollback: the OUTER raise reverts the inner workflow's writes AND suppresses both afters *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let inner = W.make "inner" (fun () -> ignore (D.insert c (person "inner"))) in
  W.after inner (fun () -> log := "inner-after" :: !log);
  let outer = W.make "outer" (fun () -> W.call inner (); ignore (D.insert c (person "outer")); raise Boom) in
  W.after outer (fun () -> log := "outer-after" :: !log);
  (try ignore (W.call outer ()) with Boom -> ());
  check "nested outer rollback reverts inner writes" (count c = 0);
  check "nested outer rollback suppresses BOTH afters" (!log = []);

  (* 5. nested commit: both afters fire only at the outer commit, inner before outer *)
  let c = D.mem (Minimongo.create ()) in
  let order = ref [] in
  let inner = W.make "inner" (fun () -> ignore (D.insert c (person "inner"))) in
  W.after inner (fun () -> order := "inner-after" :: !order);
  let outer = W.make "outer" (fun () -> W.call inner (); ignore (D.insert c (person "outer"))) in
  W.after outer (fun () -> order := "outer-after" :: !order);
  ignore (W.call outer ());
  check "nested commit runs both writes" (count c = 2);
  check "nested afters fire post-commit, inner before outer" (List.rev !order = [ "inner-after"; "outer-after" ]);

  Printf.printf "all workflow tests passed\n%!"
