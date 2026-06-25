(* The workflow runtime (Workflow.exec — what a [@workflow] function lowers to) over the in-memory
   backend: a workflow body runs in ONE transaction (rollback on raise), after-hooks fire post-commit
   with the result, before-guards veto, nested workflows flatten (the inner after fires at the OUTERMOST
   commit), and the re-entrancy guard halts a body-level reaction cycle. The [@workflow]/@after sugar is
   tested in ../ppx/test. *)

module W = Fennec_pulse_workflow.Workflow
module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let count c = List.length (D.find c (Fennec_mongo_backend.query ()))
let person n = B.doc [ ("name", B.str n) ]

exception Boom

(* a tiny stand-in for what the ppx emits for `let[@workflow] f arg = body` *)
let workflow ?(befores = []) ?(afters = []) name arg body = W.exec ~name ~befores ~afters arg body

let () =
  (* 1. after fires post-commit with the result; the body committed *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let r =
    workflow "place" ~afters:[ (fun r -> log := r :: !log) ] "ada"
      (fun () -> ignore (D.insert c (person "ada")); String.uppercase_ascii "ada")
  in
  check "exec returns the body result" (r = "ADA");
  check "body committed" (count c = 1);
  check "after fired post-commit with the result" (!log = [ "ADA" ]);

  (* 2. a raise in the body rolls it back and suppresses the after *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  (try
     ignore
       (workflow "w" ~afters:[ (fun () -> log := "after" :: !log) ] () (fun () ->
            ignore (D.insert c (person "x"));
            raise Boom))
   with Boom -> ());
  check "body raise rolls back" (count c = 0);
  check "body raise suppresses after" (!log = []);

  (* 3. a before-guard vetoes (rolls back the body) *)
  let c = D.mem (Minimongo.create ()) in
  (try ignore (workflow "w" ~befores:[ (fun () -> raise Boom) ] () (fun () -> ignore (D.insert c (person "x"))))
   with Boom -> ());
  check "before-guard veto rolls back the body" (count c = 0);

  (* 4. nested flatten: an OUTER raise reverts the inner workflow's writes AND suppresses both afters *)
  let c = D.mem (Minimongo.create ()) in
  let log = ref [] in
  let inner () =
    workflow "inner" ~afters:[ (fun () -> log := "inner" :: !log) ] () (fun () -> ignore (D.insert c (person "inner")))
  in
  (try
     ignore
       (workflow "outer" ~afters:[ (fun () -> log := "outer" :: !log) ] () (fun () ->
            inner ();
            ignore (D.insert c (person "outer"));
            raise Boom))
   with Boom -> ());
  check "nested outer rollback reverts inner writes" (count c = 0);
  check "nested outer rollback suppresses both afters" (!log = []);

  (* 5. nested commit: both afters fire post-commit, inner before outer *)
  let c = D.mem (Minimongo.create ()) in
  let order = ref [] in
  let inner () =
    workflow "inner" ~afters:[ (fun () -> order := "inner" :: !order) ] () (fun () -> ignore (D.insert c (person "inner")))
  in
  ignore
    (workflow "outer" ~afters:[ (fun () -> order := "outer" :: !order) ] () (fun () ->
         ignore (inner ());
         ignore (D.insert c (person "outer"))));
  check "nested commit runs both writes" (count c = 2);
  check "nested afters fire post-commit, inner before outer" (List.rev !order = [ "inner"; "outer" ]);

  (* 6. the re-entrancy guard halts a body-level reaction cycle (an after that re-enters its workflow) *)
  let n = ref 0 in
  let rec a () = workflow "a" ~afters:[ (fun () -> incr n; ignore (a ())) ] () (fun () -> ()) in
  ignore (a ());
  check "re-entrancy guard halts a body-level cycle at depth 1" (!n = 1);

  Printf.printf "all workflow runtime tests passed\n%!"
