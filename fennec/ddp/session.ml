(* The server-side DDP session — transport-agnostic. It consumes decoded DDP messages and produces
   them via [emit]; the websocket shell just pipes bytes. Extended (sub-tagged) mode: the server is
   stateless per session — each subscription's observe deltas are forwarded TAGGED with the sub id,
   and the client merges (DATAFLOW.md §5b). No per-session document copies, no diffing here.
   Publications and methods are caller-supplied, so this is pure and unit-testable with no socket. *)

module Msg = Message

(* a publication streams into this sink; [collection] is per-doc so one pub can feed several
   collections *)
type sink = {
  added : collection:string -> id:string -> fields:(string * Bson.t) list -> unit;
  changed :
    collection:string -> id:string -> fields:(string * Bson.t) list -> cleared:string list -> unit;
  removed : collection:string -> id:string -> unit;
  ready : unit -> unit;
}

type handle = { stop : unit -> unit }
type publication = params:Bson.t list -> sink -> handle

(* the per-call context a method runs in: the connection's current user (set by a prior method via
   [set_user_id] — e.g. a login method), and the client's randomSeed for deterministic id minting *)
type method_ctx = {
  user_id : string option;
  set_user_id : string option -> unit;
  random_seed : Bson.t option;
}

type method_fn = method_ctx -> Bson.t list -> Bson.t

type t = {
  emit : Msg.t -> unit;
  pubs : (string, publication) Hashtbl.t;
  methods : (string, method_fn) Hashtbl.t;
  subs : (string, handle) Hashtbl.t; (* subId -> running publication *)
  session_id : string;
  mutable user_id : string option; (* the connection's authenticated user (None = anonymous) *)
  (* the write fence: runs [k] once the data deltas already committed have been DELIVERED to this
     session's sink, so [updated] can never overtake the writes it announces (default: immediate) *)
  fence : (unit -> unit) -> unit;
}

let create ?user_id ?(fence = fun k -> k ()) ~session_id ~emit ~pubs ~methods () =
  { emit; pubs; methods; subs = Hashtbl.create 16; session_id; user_id; fence }

let user_id t = t.user_id

(* A method raises this for an application-level error (a code + reason the client switches on);
   the session maps it to the DDP error payload. Control exceptions are re-raised; any other
   unexpected exception becomes a generic 500. *)
exception Method_error of { code : string; reason : string }

let err code reason = { Msg.code; reason = Some reason; message = None; error_type = "Meteor.Error" }

(* ---- delta resync (v2, Sub.have) ---------------------------------------------------------------
   A resubscribing client declares what it already holds — per collection, (id -> Doc_hash of
   fields) — and this sink wrapper turns the publication's FULL initial replay into a difference:
   an [added] whose fields hash to the client's copy is SKIPPED; a held-but-different doc passes
   through (re-adds are idempotent client-side); and at [ready] every held doc the replay did NOT
   cover gets an explicit [removed] — it left the publication while the client was away. This
   REPLACES the client's quiescence pass for that resubscription (the client must not mark held
   docs tentative when it sent [have]). After [ready] the wrapper is inert. *)
let resync_wrap ~(have : (string * (string * string) list) list) (sink : sink) : sink =
  let held : (string * string, string) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (coll, ids) -> List.iter (fun (id, h) -> Hashtbl.replace held (coll, id) h) ids) have;
  let replaying = ref true in
  {
    added =
      (fun ~collection ~id ~fields ->
        if !replaying then (
          match Hashtbl.find_opt held (collection, id) with
          | Some h ->
              Hashtbl.remove held (collection, id);
              if h <> Doc_hash.fields fields then sink.added ~collection ~id ~fields
              (* matching hash: the client's copy IS current — skip the bytes *)
          | None -> sink.added ~collection ~id ~fields)
        else sink.added ~collection ~id ~fields);
    changed = sink.changed;
    removed =
      (fun ~collection ~id ->
        if !replaying then Hashtbl.remove held (collection, id);
        sink.removed ~collection ~id);
    ready =
      (fun () ->
        (* anything still held left the publication while the client was away *)
        Hashtbl.iter (fun (collection, id) _ -> sink.removed ~collection ~id) held;
        Hashtbl.reset held;
        replaying := false;
        sink.ready ());
  }

let dispatch (t : t) (m : Msg.t) : unit =
  match m with
  | Msg.Connect _ ->
      t.emit (Msg.Connected { session = t.session_id });
      t.emit (Msg.User { id = t.user_id })
  | Msg.Ping { id } -> t.emit (Msg.Pong { id })
  | Msg.Pong _ -> ()
  | Msg.Sub { id; name; params; have } -> (
      match Hashtbl.find_opt t.pubs name with
      | None -> t.emit (Msg.Nosub { id; error = Some (err "404" ("no publication " ^ name)) })
      | Some pub ->
          (* a fresh sub with an existing id replaces the old running one *)
          (match Hashtbl.find_opt t.subs id with Some h -> h.stop () | None -> ());
          let sink =
            {
              added =
                (fun ~collection ~id:docid ~fields ->
                  t.emit (Msg.Added { collection; id = docid; fields; sub = Some id }));
              changed =
                (fun ~collection ~id:docid ~fields ~cleared ->
                  t.emit (Msg.Changed { collection; id = docid; fields; cleared; sub = Some id }));
              removed =
                (fun ~collection ~id:docid ->
                  t.emit (Msg.Removed { collection; id = docid; sub = Some id }));
              ready = (fun () -> t.emit (Msg.Ready { subs = [ id ] }));
            }
          in
          (* v2 delta resync: when the client declared what it already holds, wrap the sink so the
             initial replay skips matching docs and [ready] emits explicit removed for dead ones *)
          let sink = match have with Some have -> resync_wrap ~have sink | None -> sink in
          (* a publication that raises must not hang the client in "loading" (nor unwind the read
             loop) — surface it as a Nosub error, re-raising only the control exceptions *)
          let started =
            try `Ok (pub ~params sink)
            with (Stack_overflow | Out_of_memory) as e -> raise e | _ -> `Failed
          in
          match started with
          | `Ok h -> Hashtbl.replace t.subs id h
          | `Failed ->
              t.emit (Msg.Nosub { id; error = Some (err "500" ("publication " ^ name ^ " failed")) }))
  | Msg.Unsub { id } ->
      (match Hashtbl.find_opt t.subs id with
       | Some h -> h.stop (); Hashtbl.remove t.subs id
       | None -> ());
      t.emit (Msg.Nosub { id; error = None })
  | Msg.Method { id; method_; params; random_seed } -> (
      match Hashtbl.find_opt t.methods method_ with
      | None ->
          t.emit (Msg.Result { id; error = Some (err "404" ("no method " ^ method_)); result = None });
          t.emit (Msg.Updated { methods = [ id ] })
      | Some f ->
          let ctx =
            { user_id = t.user_id;
              set_user_id =
                (fun u ->
                  if u <> t.user_id then begin
                    t.user_id <- u;
                    t.emit (Msg.User { id = u })
                  end);
              random_seed }
          in
          (match
             try Ok (f ctx params) with
             | Method_error { code; reason } -> Error (code, reason)
             | (Stack_overflow | Out_of_memory) as e -> raise e
             | _ -> Error ("500", "method failed")
           with
           | Ok v -> t.emit (Msg.Result { id; error = None; result = Some v })
           | Error (code, reason) ->
               t.emit (Msg.Result { id; error = Some (err code reason); result = None }));
          (* the write fence: [updated] is the client's cue to reveal server truth (drop its
             optimistic simulation), so it must FOLLOW the data deltas this method's writes caused *)
          t.fence (fun () -> t.emit (Msg.Updated { methods = [ id ] })))
  | _ -> ()

(* tear down all running subscriptions (connection closed) *)
let close (t : t) = Hashtbl.iter (fun _ h -> h.stop ()) t.subs; Hashtbl.clear t.subs
