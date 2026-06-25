(* The optimistic-slot BRIDGE, natively. add_task (web/methods/add_task.mlx) carries a [let%optimistic]
   slot whose body runs the SAME Task.create the handler does. The fur ppx lowers that into the method's
   stub wrapped in [Coll_writer.with_sim], so on the client the verb predicts the local cache instead of
   hitting the (absent) server backend. Here we stand in a recording Coll_writer backend (the browser
   installs a Sim-routing one), build a sim over a merge store, and run add_task's generated stub: the
   slot must call [create] exactly once, inside the call's sim context. *)

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let creates = ref 0
let saw_sim = ref false

(* the recording client backend, installed where the browser would install its Sim-routing one *)
let () =
  Coll_writer.install
    {
      Coll_writer.create =
        (fun _def v ->
          incr creates;
          if Coll_writer.current_sim () <> None then saw_sim := true;
          v);
      save = (fun _ v -> v);
      delete = (fun _ _ -> ());
      find_one = (fun _ _ -> None);
      where = (fun _ _ -> []);
      all = (fun _ -> []);
      count = (fun _ _ -> 0);
    }

let () =
  ignore Add_task.add_task;
  check "the let%optimistic slot generated a stub" (Method.stub Add_task.add_task <> None);
  let stub = Option.get (Method.stub Add_task.add_task) in
  let w = Fennec_pulse_live.Sim.writes (Fennec_pulse_live.Merge_store.create ()) ~sim:"sim1" ~seed:"seed1" in
  stub w "Buy milk";
  check "the slot's Task.create routed through Coll_writer (predicting the cache)" (!creates = 1);
  check "...inside the call's sim context (with_sim bound the ambient sim)" !saw_sim;

  (* Sim.save_t / remove_t — the typed surface a save/delete optimistic slot uses, against a real merge
     store: insert -> save (full-doc update) -> delete, the cache reflecting each step. *)
  let open Fennec_pulse_live in
  let store = Merge_store.create () in
  let w = Sim.writes store ~sim:"sim_sd" ~seed:"seed_sd" in
  let count () = Array.length (Merge_store.fetch store (Def.name Task.collection) ()) in
  let id = Sim.insert_t w Task.collection { Task.id = ""; title = "old"; body = "" } in
  check "insert_t put one row in the cache" (count () = 1);
  Sim.save_t w Task.collection { Task.id; title = "new"; body = "" };
  check "save_t kept the row (a full-doc update, not a remove)" (count () = 1);
  Sim.remove_t w Task.collection { Task.id; title = "new"; body = "" };
  check "remove_t tombstoned the row" (count () = 0);

  Printf.printf "all optimistic-slot tests passed\n%!"
