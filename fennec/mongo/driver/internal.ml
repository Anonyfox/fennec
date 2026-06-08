(* The bridge between Eio's effects-based scheduler and the blocking C driver. Each libmongoc stub
   releases the OCaml runtime lock, so running it inside an Eio systhread lets the rest of the
   program (other fibers, other systhreads) keep going while this call waits on the network. This is
   *the* reason a blocking change-stream read does not freeze the whole application. *)

let run (f : unit -> 'a) : 'a = Eio_unix.run_in_systhread f
