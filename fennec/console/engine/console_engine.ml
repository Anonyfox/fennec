(* See console_engine.mli. The in-server REPL eval host.

   This module hosts the OCaml bytecode toplevel ([Toploop]) inside the running app server. Because the
   dev server is linked with -linkall, [Toploop.execute_phrase] resolves the server's OWN linked modules
   against their live runtime symbols — so a typed-in [Pulse.find Task.collection q] runs on the live Eio
   loop and hits the live burrow, exactly like the app. This is the local half of the `iex -S mix
   phx.server` model; no second process, no reloaded copy, no parallel build.

   It registers itself through {!Fennec.set_console_hook} so fennec's core never names [compiler-libs];
   the prod native build links neither this module nor the compiler. *)

module P = Console_protocol

(* ── the toplevel: initialise once, against this exe's linked modules ─────────────────────────────── *)

(* a directory holding compiled interface (.cmi) files the typechecker needs. The runtime symbols are
   already linked (-linkall); the toplevel only needs the .cmi's on its search path to TYPE a phrase. *)
let split_colon s = String.split_on_char ':' s |> List.filter (fun d -> d <> "")

(* fallback when the CLI didn't hand us the dirs: walk the running exe's [_build/<profile>] tree for
   every [*.objs/byte] (dune's per-library interface dir). Covers the monorepo; downstream apps get the
   authoritative set from [FENNEC_CONSOLE_CMIDIRS] (computed CLI-side via `dune ocaml top`). *)
let build_objs_dirs () =
  let exe = try Sys.executable_name with _ -> "" in
  (* find the "_build/<profile>" ancestor of the exe path *)
  let rec build_root dir =
    if dir = "/" || dir = "." || dir = "" then None
    else if Filename.basename dir = "_build" then Some dir
    else build_root (Filename.dirname dir)
  in
  match build_root (Filename.dirname exe) with
  | None -> []
  | Some build ->
    let acc = ref [] in
    let rec walk dir =
      match Sys.readdir dir with
      | exception _ -> ()
      | entries ->
        if Filename.check_suffix dir ".objs/byte" then acc := dir :: !acc;
        Array.iter
          (fun name ->
            let p = Filename.concat dir name in
            if (try Sys.is_directory p with _ -> false) && name <> "_build" then walk p)
          entries
    in
    walk build;
    !acc

(* the CLI hands us the authoritative dir list (from `dune ocaml top`) via this env; else we fall back
   to walking [_build] ourselves. Keeps the engine working both ways. *)
let load_path_dirs () =
  match Sys.getenv_opt "FENNEC_CONSOLE_CMIDIRS" with
  | Some s when String.trim s <> "" -> split_colon s
  | _ -> build_objs_dirs ()

let initialised = ref false

let init_toplevel () =
  if not !initialised then begin
    initialised := true;
    Sys.interactive := true;
    Sys.catch_break true;       (* let a runaway pure-CPU phrase raise [Break] on a delivered SIGINT *)
    Topeval.init ();            (* register THIS exe's linked globals (needs the server's -linkall) *)
    Compmisc.init_path ();
    Toploop.initialize_toplevel_env ();
    Toploop.set_paths ();
    List.iter (fun d -> try Topdirs.dir_directory d with _ -> ()) (load_path_dirs ())
  end

(* ── evaluate one phrase, capturing its result/type/errors ───────────────────────────────────────── *)

(* The toplevel writes the result line ([- : t = v]) and type errors to the formatter we pass; the
   phrase's own [print_*] side effects go to the process stdout (drained + rendered by the driver), so we
   do NOT redirect file descriptors — that would also swallow concurrent HTTP logs under [dev --console].
   Returns the rendered output and whether it was an error. *)
let eval (src : string) : string * bool =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  let is_error = ref false in
  let src = String.trim src in
  let src = if src = "" then ";;" else if Filename.check_suffix src ";;" then src else src ^ ";;" in
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "//console";
  (try
     let rec run () =
       match !Toploop.parse_toplevel_phrase lexbuf with
       | exception End_of_file -> ()
       | phrase ->
         if not (Toploop.execute_phrase true fmt phrase) then is_error := true;
         run ()
     in
     run ()
   with
   | Eio.Cancel.Cancelled _ as e -> raise e (* let Eio unwind a cancelled eval *)
   | Sys.Break -> is_error := true; Format.fprintf fmt "Interrupted.@."
   | e ->
     is_error := true;
     (try Location.report_exception fmt e with _ -> Format.fprintf fmt "%s@." (Printexc.to_string e)));
  Format.pp_print_flush fmt ();
  (String.trim (Buffer.contents buf), !is_error)

(* ── completion: enumerate the toplevel typing environment ───────────────────────────────────────── *)

let longident_of_string s =
  match String.split_on_char '.' s with
  | [] -> None
  | first :: rest ->
    (* OCaml 5.4's [Ldot]/[Lapply] carry located sub-parts; wrap with no-loc *)
    Some
      (List.fold_left
         (fun acc part -> Longident.Ldot (Location.mknoloc acc, Location.mknoloc part))
         (Longident.Lident first) rest)

let starts_with s p = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* completions for [prefix]: split at the last '.', look up the module path (if any), keep value /
   module / constructor names that begin with the leaf. Best-effort — any compiler-API hiccup yields []. *)
let complete (prefix : string) : string list =
  try
    let modpath, leaf =
      match String.rindex_opt prefix '.' with
      | Some i -> (longident_of_string (String.sub prefix 0 i), String.sub prefix (i + 1) (String.length prefix - i - 1))
      | None -> (None, prefix)
    in
    let env = !Toploop.toplevel_env in
    let acc = ref [] in
    let keep name = if starts_with name leaf && name <> "" then acc := name :: !acc in
    Env.fold_values (fun name _ _ () -> keep name) modpath env ();
    Env.fold_modules (fun name _ _ () -> keep name) modpath env ();
    List.sort_uniq compare !acc
  with _ -> []

(* ── the boot preamble: a ready-to-use environment ───────────────────────────────────────────────── *)

(* The modules opened so a phrase reads bare [Conn]/[Endpoint]/[Pulse]/… like app code. Returned to the
   driver in the banner so the human sees what is in scope. *)
let opened_modules = [ "Fennec"; "Paw" ]

let run_preamble () =
  List.iter (fun m -> ignore (eval ("open " ^ m))) opened_modules

(* ── the protocol server: one connection = one REPL session ──────────────────────────────────────── *)

let backend_label () =
  match Sys.getenv_opt "MONGO_URL" with Some u when String.trim u <> "" -> u | _ -> ":memory:"

let banner () : P.banner =
  { app = (try Filename.basename (Sys.getcwd ()) with _ -> "app");
    backend = backend_label ();
    version = "0.0.1";
    opened = opened_modules }

(* serialise writes (the eval fiber and the reader fiber both reply) so frames never interleave *)
let handle_connection ~sw flow =
  let write_mutex = Eio.Mutex.create () in
  let write (r : P.reply) =
    Eio.Mutex.use_rw ~protect:true write_mutex (fun () -> Eio.Flow.copy_string (P.frame_reply r) flow)
  in
  (* one eval may be in flight; remember how to cancel it for a [Cancel] frame *)
  let current : (int * (unit -> unit)) option ref = ref None in
  let reader = P.Reader.for_requests () in
  let chunk = Cstruct.create 4096 in
  (* read → feed → drain complete frames; dispatch each. The reader fiber NEVER blocks on an eval (evals
     are forked), so a [Cancel] arriving mid-eval is read and acted on immediately. *)
  let dispatch (req : P.request) =
    match req with
    | P.Hello _ -> write (P.Banner (banner ()))
    | P.Complete { id; prefix } -> write (P.Completions { id; items = complete prefix })
    | P.Cancel { id } -> (match !current with Some (cid, cancel) when cid = id -> cancel () | _ -> ())
    | P.Eval { id; phrase } ->
      Eio.Fiber.fork ~sw (fun () ->
          (try
             Eio.Switch.run (fun esw ->
                 current := Some (id, fun () -> Eio.Switch.fail esw Exit);
                 let text, is_error = eval phrase in
                 if text <> "" then write (P.Output { id; text; is_error }))
           with
           | Eio.Cancel.Cancelled _ | Exit ->
             write (P.Output { id; text = "^C interrupted"; is_error = true }));
          current := None;
          write (P.Done { id }))
  in
  let rec pump () =
    match P.Reader.next reader with
    | Some req -> dispatch req; pump ()
    | None -> (
      match Eio.Flow.single_read flow chunk with
      | exception End_of_file -> () (* client disconnected — end this session *)
      | exception _ -> ()
      | n ->
        P.Reader.feed reader (Cstruct.to_string (Cstruct.sub chunk 0 n));
        pump ())
  in
  pump ()

(* ── start: the dev hook fennec invokes (only when a socket path is provided) ─────────────────────── *)

let start : Fennec.console_start =
 fun ~sw ~net ~sleep:_ ~endpoints ->
  ignore endpoints;
  match Sys.getenv_opt Paw.Dev_proto.env_console_sock with
  | None | Some "" -> () (* no socket requested → stay idle (a plain `dune exec` server has no console) *)
  | Some path ->
    init_toplevel ();
    run_preamble ();
    (match try Some (Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true net (`Unix path)) with _ -> None with
     | None -> () (* couldn't bind (path too long / stale) — the console is simply unavailable *)
     | Some socket ->
       Eio.Fiber.fork ~sw (fun () ->
           Eio.Net.run_server socket
             (fun flow _addr -> Eio.Switch.run (fun csw -> handle_connection ~sw:csw flow))
             ~on_error:(fun _ -> ())))

(* register with fennec at module-load time. Because the dev server links this lib (with -linkall), this
   side effect runs at startup and [Fennec.serve] finds the hook; the prod build links nothing here. *)
let () = Fennec.set_console_hook start

(* ── tests ───────────────────────────────────────────────────────────────────────────────────────── *)

let%test "eval of a stdlib phrase captures the result line" =
  init_toplevel ();
  let text, is_error = eval "1 + 1" in
  (not is_error) && Fennec_hunt_unit.str_contains text "2"

let%test "eval of a type error reports it as an error, not a crash" =
  init_toplevel ();
  let _text, is_error = eval "1 + true" in
  is_error
