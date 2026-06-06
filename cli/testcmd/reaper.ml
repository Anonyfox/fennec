(* The registry is an IMMUTABLE list behind a ref. Writers (track/untrack) serialize via the
   mutex and swap in a fresh list; [kill_all] reads the ref WITHOUT the lock — an atomic pointer
   read that yields a consistent (if momentarily stale) list. That keeps the signal handler
   deadlock-free: it never has to acquire a lock a worker thread might be holding. *)
let m = Mutex.create ()
let live : int list ref = ref []

let track pid =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) (fun () -> live := pid :: !live)

let untrack pid =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) (fun () -> live := List.filter (fun p -> p <> pid) !live)

let kill_all () = List.iter (fun pid -> try Unix.kill pid Sys.sigkill with _ -> ()) !live

let installed = ref false

let install_signal_handlers () =
  if not !installed then begin
    installed := true;
    let handle s = kill_all (); exit (if s = Sys.sigterm then 143 else 130) in
    (try Sys.set_signal Sys.sigint (Sys.Signal_handle handle) with _ -> ());
    (try Sys.set_signal Sys.sigterm (Sys.Signal_handle handle) with _ -> ())
  end
