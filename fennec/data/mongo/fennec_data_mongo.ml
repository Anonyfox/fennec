(* Native MongoDB backend — a {!Fennec_data.Backend.S} over the statically-linked libmongoc driver,
   so the reactive/DDP/realtime stack runs over a real mongod with no other change (it is the same
   seam Minimongo implements in memory).

   Every blocking driver call runs in an Eio systhread (each libmongoc stub releases the OCaml
   runtime lock), so a mongo round-trip suspends only the calling fiber, not the scheduler. CRUD that
   the typed FFI does not cover directly (multi/upsert update, multi-delete, count) goes through the
   driver's generic [command]. [observe_changes] polls + diffs: it works against a standalone mongod
   (change streams, which need a replica set, are a later optimization). *)

module Ffi = Fennec_mongo_ffi.Mongo_ffi
module BJ = Fennec_mongo_bson_json.Bson_json
module Diff = Query.Diff
module Id = Query.Id
module Backend = Fennec_data.Backend
module B = Bson

let available = Ffi.available

type connection = Ffi.pool

let connect uri =
  Ffi.init ();
  Ffi.pool_new uri

type collection = {
  pool : Ffi.pool;
  db : string;
  name : string;
  sw : Eio.Switch.t; (* observe forks its polling loop here *)
  sleep : float -> unit; (* observe's inter-poll sleep (an Eio clock sleep) *)
  poll : float;
}

let collection ?(poll = 0.5) ~sw ~sleep conn ~db ~name = { pool = conn; db; name; sw; sleep; poll }

(* run a blocking driver call off the scheduler *)
let run = Eio_unix.run_in_systhread
let empty_doc = function B.Document [] -> true | _ -> false

let int_field reply k =
  match B.get reply k with Some v -> ( match B.as_float v with Some f -> int_of_float f | None -> 0) | None -> 0

let command c (cmd : B.t) = BJ.of_string (run (fun () -> Ffi.command c.pool c.db (BJ.to_string cmd)))

(* ---- Backend.S ---------------------------------------------------------- *)

let insert c (d : B.t) =
  let kvs = Diff.kvs_of d in
  let id, doc =
    match List.assoc_opt "_id" kvs with
    | Some v -> (Diff.id_to_string v, d)
    | None ->
        let id = Id.random_id () in
        (id, B.Document (("_id", B.String id) :: kvs))
  in
  ignore (run (fun () -> Ffi.insert_one c.pool c.db c.name (BJ.to_string doc)));
  id

let update c ~multi ~upsert sel m =
  let reply =
    command c
      (B.doc
         [ ("update", B.str c.name);
           ( "updates",
             B.array [ B.doc [ ("q", sel); ("u", m); ("multi", B.bool multi); ("upsert", B.bool upsert) ] ] ) ])
  in
  int_field reply "n"

let remove c sel =
  let reply =
    command c (B.doc [ ("delete", B.str c.name); ("deletes", B.array [ B.doc [ ("q", sel); ("limit", B.int 0) ] ]) ])
  in
  int_field reply "n"

let opts_of (q : Backend.query) =
  B.Document
    (List.filter_map Fun.id
       [ (if empty_doc q.sort then None else Some ("sort", q.sort));
         (if q.skip > 0 then Some ("skip", B.int q.skip) else None);
         (if q.limit > 0 then Some ("limit", B.int q.limit) else None);
         (if empty_doc q.fields then None else Some ("projection", q.fields)) ])

let find c (q : Backend.query) =
  BJ.list_of_string (run (fun () -> Ffi.find c.pool c.db c.name (BJ.to_string q.selector) (BJ.to_string (opts_of q))))

let find_one c (q : Backend.query) = match find c { q with Backend.limit = 1 } with x :: _ -> Some x | [] -> None
let count c sel = int_field (command c (B.doc [ ("count", B.str c.name); ("query", sel) ])) "n"

(* the pipeline is a JSON array of stage docs; the C side wraps it as {pipeline: …} for the driver *)
let aggregate c (pipeline : B.t list) =
  BJ.list_of_string (run (fun () -> Ffi.aggregate c.pool c.db c.name (BJ.to_string (B.Array pipeline)) "{}"))

(* observe via polling: initial snapshot, then re-find + per-id field diff each tick *)
let observe_changes c (q : Backend.query) ~added ~changed ~removed : Backend.handle =
  let snap () = find c { q with Backend.skip = 0; limit = 0; sort = B.Document [] } in
  let index docs =
    let h = Hashtbl.create 64 in
    List.iter (fun d -> Hashtbl.replace h (Diff.doc_id d) d) docs;
    h
  in
  let stopped = ref false in
  (* The contract (Reactive.run_publication): existing docs are replayed as [added] SYNCHRONOUSLY
     during registration, because the caller signals DDP [ready] the instant this returns — so the
     client must see the initial set BEFORE ready (Minimongo does exactly this). Hence: take the
     first snapshot and fire its [added] here, in the caller's fiber, BEFORE forking — then fork only
     the polling loop. (The snapshot's [find] suspends this fiber on its systhread, so without this
     split the initial [added] would run only after [run_publication] had already returned and fired
     [ready] — i.e. ready-before-data, the bug this avoids.) *)
  let prev = ref (index (snap ())) in
  Hashtbl.iter (fun id d -> added id (Diff.fields_without_id d)) !prev;
  Eio.Fiber.fork ~sw:c.sw (fun () ->
      while not !stopped do
        c.sleep c.poll;
        if not !stopped then begin
          let cur = index (snap ()) in
          Hashtbl.iter (fun id _ -> if not (Hashtbl.mem cur id) then removed id) !prev;
          Hashtbl.iter
            (fun id d ->
              match Hashtbl.find_opt !prev id with
              | None -> added id (Diff.fields_without_id d)
              | Some old -> (
                  match Diff.diff_fields ~old_doc:old ~new_doc:d with
                  | [], [] -> ()
                  | upd, cleared -> changed id (B.Document upd) cleared))
            cur;
          prev := cur
        end
      done);
  { Backend.stop = (fun () -> stopped := true) }

(* alias the native collection type so [Dynamic] can name it (its own [collection] shadows it) *)
type mongo_collection = collection

(* A runtime-selectable backend: in-memory OR this native driver, behind ONE Backend.S, so an app
   picks at boot (real mongo if configured, else :memory:) with no type change downstream. The
   per-op dispatch references the outer (native) ops — they are not shadowed because each [let] here
   is non-recursive. *)
module Dynamic = struct
  module Mini = Backend.Mini

  type collection = Mem of Minimongo.t | Real of mongo_collection

  let mem m = Mem m
  let real ?poll ~sw ~sleep conn ~db ~name = Real (collection ?poll ~sw ~sleep conn ~db ~name)

  (* The convention the fennec CLI speaks: `fennec dev --mongo` / `fennec test --mongo` launch a
     managed mongod and export its URL as MONGO_URL. [from_env] is the whole app-side story — real
     mongo when it's set, a fresh in-memory engine otherwise — so an app carries no config branch of
     its own. Build it in [Fennec.serve ~on_start] so the [sw]/[sleep] it captures drive the
     observe_changes polling loop. *)
  let mongo_url_env = "MONGO_URL"

  let from_env ?poll ~sw ~sleep ~db ~name () =
    match Sys.getenv_opt mongo_url_env with
    | Some url when String.trim url <> "" -> real ?poll ~sw ~sleep (connect url) ~db ~name
    | _ -> mem (Minimongo.create ())

  let insert c d = match c with Mem m -> Mini.insert m d | Real r -> insert r d
  let update c ~multi ~upsert s m = match c with Mem mm -> Mini.update mm ~multi ~upsert s m | Real r -> update r ~multi ~upsert s m
  let remove c s = match c with Mem m -> Mini.remove m s | Real r -> remove r s
  let find c q = match c with Mem m -> Mini.find m q | Real r -> find r q
  let find_one c q = match c with Mem m -> Mini.find_one m q | Real r -> find_one r q
  let count c s = match c with Mem m -> Mini.count m s | Real r -> count r s
  let aggregate c p = match c with Mem m -> Mini.aggregate m p | Real r -> aggregate r p

  let observe_changes c q ~added ~changed ~removed =
    match c with
    | Mem m -> Mini.observe_changes m q ~added ~changed ~removed
    | Real r -> observe_changes r q ~added ~changed ~removed
end
