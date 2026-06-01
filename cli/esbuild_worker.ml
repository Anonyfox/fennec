(* The dev-mode esbuild build worker.

   `fennec build` does a COLD esbuild build each invocation: create a context, build
   once, dispose. The context-creation parse work is the bulk of the cost (~0.4s for
   a small app). In the dev loop that cost is paid on every keystroke-triggered
   rebuild. esbuild's real strength is a WARM context whose [Rebuild] reuses parse
   work — single-digit milliseconds.

   But dune drives bundling as one-shot rule actions (`fennec build …`), each a fresh
   process: nowhere to keep a context warm. So `fennec dev` starts THIS worker — a
   long-lived process holding one warm context per bundle — and exports its socket.
   In dev, `fennec build` delegates to the worker (a warm [Rebuild]); everywhere else
   (prod, plain `dune build`, or ANY worker failure) it falls back to the cold path.
   dune still owns the build graph and runs the rule; the worker only accelerates the
   action, exactly like a compiler server (cf. Bazel persistent workers).

   RELIABILITY CONTRACT:
   - The warm path MUST be byte-identical to the cold path. It is, by construction:
     the worker creates its context from the SAME options JSON the cold path would
     ([Esbuild.json_opts]), so [Rebuild] and a one-shot [build] run identical esbuild.
   - The worker is a pure accelerator. The client returns [Some bytes] ONLY on a
     clean success; EVERY other outcome (no socket, dead socket, timeout, protocol
     error, build error, internal error) returns [None] and the caller falls back to
     the cold path — which then produces the authoritative result (success or the
     real error). Worst case = today's behavior.
   - The worker never crashes on a bad client: each connection is handled under a
     catch-all. It self-terminates if orphaned (parent `fennec dev` gone) and cleans
     up its socket on exit. *)

(* ---- length-prefixed framing over the unix socket ---- *)

let write_all fd (s : string) =
  let n = String.length s in
  let rec go off =
    if off < n then begin
      let w = Unix.write_substring fd s off (n - off) in
      if w <= 0 then failwith "short write";
      go (off + w)
    end
  in
  go 0

let read_exact fd n =
  let b = Bytes.create n in
  let rec go off =
    if off < n then begin
      let r = Unix.read fd b off (n - off) in
      if r <= 0 then failwith "eof";
      go (off + r)
    end
  in
  go 0;
  Bytes.unsafe_to_string b

let put_u32 (n : int) : string =
  let b = Bytes.create 4 in
  Bytes.set_uint8 b 0 ((n lsr 24) land 0xff);
  Bytes.set_uint8 b 1 ((n lsr 16) land 0xff);
  Bytes.set_uint8 b 2 ((n lsr 8) land 0xff);
  Bytes.set_uint8 b 3 (n land 0xff);
  Bytes.unsafe_to_string b

let get_u32 (s : string) : int =
  (Char.code s.[0] lsl 24) lor (Char.code s.[1] lsl 16) lor (Char.code s.[2] lsl 8) lor Char.code s.[3]

(* Protocol:
   request  = [u32 len][opts-json bytes]
   response = [1 status byte][u32 len][payload]
              status 'O' -> payload is the bundle bytes
              status 'E' -> payload is a build-error message
              status 'X' -> payload is an internal-error message *)

(* ---- client (used by `fennec build`) ---- *)

(* Ask the worker to (re)build the bundle for [opts_json]. Returns [Some bytes] only
   on a clean success; [None] on anything else, so the caller falls back to cold. *)
let client_build ~socket ~opts_json : string option =
  match (try Some (Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0) with _ -> None) with
  | None -> None
  | Some fd ->
    Fun.protect
      ~finally:(fun () -> try Unix.close fd with _ -> ())
      (fun () ->
        try
          (* generous timeouts: a worker's FIRST build of a bundle creates the context
             (~0.4s); warm rebuilds are ms. If the worker is wedged we eat one timeout
             then fall back to cold. *)
          Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.0;
          Unix.setsockopt_float fd Unix.SO_SNDTIMEO 10.0;
          Unix.connect fd (Unix.ADDR_UNIX socket);
          write_all fd (put_u32 (String.length opts_json));
          write_all fd opts_json;
          let status = read_exact fd 1 in
          let len = get_u32 (read_exact fd 4) in
          let payload = if len > 0 then read_exact fd len else "" in
          if status = "O" then Some payload else None
        with _ -> None)

(* ---- server (the `__esbuild-worker` subcommand) ---- *)

let serve ~socket =
  (try Unix.unlink socket with _ -> ()); (* clear a stale socket from a prior run *)
  let listen_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind listen_fd (Unix.ADDR_UNIX socket);
  Unix.listen listen_fd 16;
  (* one warm context per distinct options JSON (i.e. per bundle) *)
  let ctxs : (string, Fennec_buildkit.Esbuild.ctx) Hashtbl.t = Hashtbl.create 8 in
  let cleanup () =
    (try Unix.close listen_fd with _ -> ());
    try Unix.unlink socket with _ -> ()
  in
  let shutdown _ = cleanup (); exit 0 in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown);
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown);
  let reply fd status payload =
    write_all fd (String.make 1 status);
    write_all fd (put_u32 (String.length payload));
    write_all fd payload
  in
  let get_or_create opts_json =
    match Hashtbl.find_opt ctxs opts_json with
    | Some c -> c
    | None ->
      let c = Fennec_buildkit.Esbuild.create_json opts_json in
      Hashtbl.replace ctxs opts_json c;
      c
  in
  let handle fd =
    match
      let len = get_u32 (read_exact fd 4) in
      let opts_json = read_exact fd len in
      Fennec_buildkit.Esbuild.rebuild (get_or_create opts_json)
    with
    | bytes -> reply fd 'O' bytes
    | exception Failure m -> reply fd 'E' m (* build error / create failure *)
    | exception e -> reply fd 'X' (Printexc.to_string e)
  in
  let rec loop () =
    (match (try Unix.select [ listen_fd ] [] [] 2.0 with _ -> ([], [], [])) with
     | [], _, _ -> if Unix.getppid () = 1 then shutdown () (* orphaned: `fennec dev` is gone *)
     | _ -> (
       match (try Some (Unix.accept listen_fd) with _ -> None) with
       | None -> ()
       | Some (cfd, _) ->
         (* a single bad client must never take down the worker *)
         (try Fun.protect ~finally:(fun () -> try Unix.close cfd with _ -> ()) (fun () -> handle cfd)
          with _ -> ())));
    loop ()
  in
  loop ()
