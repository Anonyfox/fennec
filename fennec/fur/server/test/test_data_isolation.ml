(* DD7: the SSR data context (Fur.Data's seed table + fetch source) is fiber-local on the native
   server, so two concurrent renders never share state. Proven by interleaving two fibers: each
   writes a value, yields to let the other run, then reads back — and sees ONLY its own, despite
   Fur.Data being one module. Outside any context, the process-global fallback is independent. *)

let pairs () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) (Fur.Data.seed_table ()) []

let () =
  Eio_main.run @@ fun _env ->
  let a_saw = ref [] and b_saw = ref [] in
  Eio.Fiber.both
    (fun () ->
      Fur.Data.with_context (fun () ->
          Fur.Data.put_seed "k" "A";
          Eio.Fiber.yield ();
          (* B interleaves here, writing "B" into ITS own context *)
          a_saw := pairs ()))
    (fun () ->
      Fur.Data.with_context (fun () ->
          Fur.Data.put_seed "k" "B";
          Eio.Fiber.yield ();
          b_saw := pairs ()));
  (* each fiber saw ONLY its own write — proof the seed table is per-fiber, not shared *)
  assert (!a_saw = [ ("k", "A") ]);
  assert (!b_saw = [ ("k", "B") ]);
  (* outside any context: the process-global fallback, untouched by either render *)
  assert (pairs () = []);
  print_endline "fiber-local SSR data context: isolation holds under concurrent renders — PASS"
