(* The data verbs (create / save / delete / get / all / find) + transaction end to end over the ambient
   in-memory backend (MONGO_URL=:memory: under a real Eio switch, which also exercises Tx's Eio path).
   create mints an id and persists; save persists a change (the transition mechanism); transaction rolls
   back on raise; delete removes. The [@workflow]/@after sugar is tested in the workflow ppx test. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Pulse = Fennec_pulse_app

type t = { id : string; name : string; status : string } [@@deriving model]

let collection = Def.v "widgets" codec
let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Fennec_mongo_dynamic.Dynamic.set_switch sw;

  (* create returns the stored aggregate with a minted id *)
  let w = Pulse.create collection { id = ""; name = "a"; status = "new" } in
  check "create mints an id" (w.id <> "");
  check "create persisted" (match Pulse.get collection w.id with Some x -> x.name = "a" && x.status = "new" | None -> false);

  (* save persists a full aggregate by _id (the transition mechanism) and returns it *)
  let w2 = Pulse.save collection { w with status = "active" } in
  check "save returns the new value" (w2.status = "active");
  check "save persisted by _id" (match Pulse.get collection w.id with Some x -> x.status = "active" | None -> false);

  (* transaction rolls back on raise — the create inside vanishes (atomicity at the facade level) *)
  (try Pulse.transaction (fun () -> ignore (Pulse.create collection { id = ""; name = "ghost"; status = "x" }); failwith "boom")
   with Failure _ -> ());
  check "transaction rollback reverts the create" (List.length (Pulse.all collection) = 1);

  (* delete removes by id *)
  Pulse.delete collection w2;
  check "delete removed it" (Pulse.get collection w.id = None);

  (* all / find one-shot reads *)
  ignore (Pulse.create collection { id = ""; name = "x"; status = "new" });
  ignore (Pulse.create collection { id = ""; name = "y"; status = "active" });
  check "all returns every aggregate" (List.length (Pulse.all collection) = 2);
  check "find ~where filters" (List.length (Pulse.find collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "new") ]) ]) = 1);

  Printf.printf "all facade data-verb tests passed\n%!"
