let map ~jobs f xs =
  let arr = Array.of_list xs in
  let n = Array.length arr in
  if n <= 1 || jobs <= 1 then
    (* serial — explicit left-to-right so side effects run in input order (stdlib List.map
       leaves evaluation order unspecified), results returned in order *)
    List.rev (List.fold_left (fun acc x -> f x :: acc) [] xs)
  else begin
    (* one slot per input; a thread fills its own slot, so no slot is shared between threads *)
    let slots = Array.make n None in
    (* acquiring on the MAIN thread before spawning throttles creation: when [jobs] threads are
       live the main thread blocks here until one releases, so at most [jobs] run at once *)
    let sem = Semaphore.Counting.make jobs in
    let spawn i x =
      Semaphore.Counting.acquire sem;
      Thread.create
        (fun () ->
          Fun.protect
            ~finally:(fun () -> Semaphore.Counting.release sem)
            (fun () -> slots.(i) <- Some (try Ok (f x) with e -> Error e)))
        ()
    in
    let threads = Array.mapi spawn arr in
    Array.iter Thread.join threads;
    Array.to_list
      (Array.map
         (function Some (Ok r) -> r | Some (Error e) -> raise e | None -> assert false)
         slots)
  end
