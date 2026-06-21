(* `fennec agent selftest`: exercise the full emit→hook loop for EVERY registered adapter against a
   throwaway journal, so a harness's wiring — its injection shape especially — can't silently rot.
   Pure + offline: no dev server, no real harness, no global config touched. *)

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    ("fennec-selftest-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_float (Unix.gettimeofday ()))

(* run one adapter through the loop: emit a verdict, render it as that harness's injected feedback,
   and confirm the verdict text survived AND the output is a wrapped JSON object (not the bare text). *)
let probe (h : Harness.t) =
  let dir = temp_dir () in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      let t = Journal.start ~dir ~root:"selftest" () in
      Journal.emit t ~kind:"reload" ~summary:"selftest verdict" ();
      let out = Hook.run ~harness:h ~dir ~timeout:0.5 ~input:"{}" in
      let carries = Harness.contains out "selftest verdict" in
      (* a JSON object wrapper (each adapter's exact field shape is unit-tested in its own module) *)
      let wrapped = String.length out > 1 && out.[0] = '{' && out.[String.length out - 1] = '}' in
      (h.id, carries && wrapped))

let run () = List.map probe Registry.all

let%test "selftest passes for every registered adapter" = List.for_all snd (run ())
