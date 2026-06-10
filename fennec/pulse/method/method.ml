(* A typed method — ONE shared value carrying the wire name, the arg/result codecs, and the optional
   latency-compensation stub. Declared once in shared code; the server attaches its handler to the
   value (Reactive.handle), the client calls through it (Ddp_client.call_m) — so a name typo, an
   arity change, or a codec drift is a COMPILE error across the whole app, not a runtime 404.

   The stub is the opt-in optimistic half: it runs ONLY in the browser, against the client cache,
   through the [sim_writes] surface — server truth replaces it when the method's [updated] arrives.
   Stubs are separate from handlers on purpose: real handlers do auth/secrets/server-only validation
   and are not simulations. *)

type sim_writes = {
  insert : string -> Bson.t -> string; (* collection -> doc -> minted _id (seed-deterministic) *)
  update : string -> Bson.t -> Bson.t -> int; (* collection -> selector -> modifier -> matched *)
  remove : string -> Bson.t -> int; (* collection -> selector -> removed *)
}

type ('a, 'r) t = {
  name : string;
  args : 'a Codec.args;
  result : 'r Codec.t;
  stub : (sim_writes -> 'a -> unit) option;
}

(* the stub-replay registry (PWA tier 3): a persisted outbox entry carries only (name, params, seed)
   — closures don't survive a reload — so [define] registers a replayer that decodes the persisted
   params and re-runs the stub. With the deterministic seed streams, a restored simulation reminted
   after a reload is byte-identical to the original. *)
let _registry : (string, (Bson.t list -> sim_writes -> unit) option) Hashtbl.t = Hashtbl.create 32
let _reg_lock = Mutex.create ()

let define ?stub name ~args ~result =
  Mutex.lock _reg_lock;
  Hashtbl.replace _registry name
    (Option.map
       (fun s (params : Bson.t list) (sim : sim_writes) ->
         match args.Codec.dec_args params with Ok a -> s sim a | Error _ -> ())
       stub);
  Mutex.unlock _reg_lock;
  { name; args; result; stub }

let stub_replay name =
  Mutex.lock _reg_lock;
  let r = match Hashtbl.find_opt _registry name with Some v -> v | None -> None in
  Mutex.unlock _reg_lock;
  r

let name m = m.name
let args m = m.args
let result m = m.result
let stub m = m.stub

(* ---- deterministic id streams (latency compensation) -------------------------------------------
   The client sends a random seed with the method call; BOTH sides mint insert ids from the same
   (seed, collection) stream — so the stub's optimistic document and the server's real one share an
   _id and converge to a single row in the client cache (no duplicate-then-vanish flicker). One
   stream per collection, so differing insert interleavings ACROSS collections still converge; the
   per-collection insert ORDER must match between stub and handler (document it, like Meteor).
   splitmix64 over an FNV-1a 64 hash of seed+scope — pure OCaml, identical native and jsoo. *)
module Seed = struct
  let fnv64 (s : string) : int64 =
    let prime = 0x100000001b3L in
    let h = ref 0xcbf29ce484222325L in
    String.iter (fun c -> h := Int64.mul (Int64.logxor !h (Int64.of_int (Char.code c))) prime) s;
    !h

  let splitmix_next (state : int64 ref) : int64 =
    state := Int64.add !state 0x9E3779B97F4A7C15L;
    let z = !state in
    let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 30)) 0xBF58476D1CE4E5B9L in
    let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 27)) 0x94D049BB133111EBL in
    Int64.logxor z (Int64.shift_right_logical z 31)

  (* [stream ~seed ~scope] is an [rng : int -> int] (bound -> value in [0, bound)) suitable for
     [Query.Id.random_id ?rng] / [object_id ?rng]; same (seed, scope) ⇒ same sequence, both sides *)
  let stream ~seed ~scope : int -> int =
    let state = ref (fnv64 (seed ^ "\x00" ^ scope)) in
    fun bound ->
      if bound <= 0 then 0
      else
        let v = Int64.rem (splitmix_next state) (Int64.of_int bound) in
        Int64.to_int (if Int64.compare v 0L < 0 then Int64.add v (Int64.of_int bound) else v)
end
