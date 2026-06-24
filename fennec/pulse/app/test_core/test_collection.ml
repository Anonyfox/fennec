(* The Collections write-API end to end over the ambient in-memory backend (MONGO_URL=:memory: under a
   real Eio switch — which also exercises Tx's Eio path, complementing the workflow unit tests that hit
   its no-scheduler fallback). create mints an id and persists; a named transition changes + persists
   state and fires its @after post-commit; a transition that raises vetoes (state intact, no @after);
   delete removes by id. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Pulse = Fennec_pulse_app
module C = Pulse.Collection
module W = Pulse.Workflow

type t = { id : string; name : string; status : string } [@@deriving model]

let collection = Def.v "widgets" codec
let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Fennec_mongo_dynamic.Dynamic.set_switch sw;

  (* create returns the stored aggregate with a minted id *)
  let w = W.call (C.create collection) { id = ""; name = "a"; status = "new" } in
  check "create mints an id" (w.id <> "");
  check "create persisted" (match C.get collection w.id with Some x -> x.name = "a" && x.status = "new" | None -> false);

  (* a named transition changes + persists state, and its @after fires post-commit with the result *)
  let activate =
    C.transition collection "activate" (fun w ->
        if w.status <> "new" then failwith "not new";
        { w with status = "active" })
  in
  let fired = ref [] in
  W.after activate (fun w -> fired := w.status :: !fired);
  let w2 = W.call activate w in
  check "transition returns new state" (w2.status = "active");
  check "transition persisted" (match C.get collection w.id with Some x -> x.status = "active" | None -> false);
  check "transition @after fired with the result" (!fired = [ "active" ]);

  (* a transition that raises vetoes: state unchanged, no @after *)
  let break = C.transition collection "break" (fun _ -> failwith "nope") in
  W.after break (fun _ -> fired := "broke" :: !fired);
  (try ignore (W.call break w2) with Failure _ -> ());
  check "veto left the persisted state intact" (match C.get collection w.id with Some x -> x.status = "active" | None -> false);
  check "veto fired no @after" (not (List.mem "broke" !fired));

  (* delete removes by id *)
  ignore (W.call (C.delete collection) w2);
  check "delete removed it" (C.get collection w.id = None);

  (* all / find one-shot reads *)
  ignore (W.call (C.create collection) { id = ""; name = "x"; status = "new" });
  ignore (W.call (C.create collection) { id = ""; name = "y"; status = "active" });
  check "all returns every aggregate" (List.length (C.all collection) = 2);
  check "find ~where filters" (List.length (C.find collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "new") ]) ]) = 1);

  Printf.printf "all collection tests passed\n%!"
