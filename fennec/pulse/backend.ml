(* The storage backend a reactive Meteor surface needs: anything that can do CRUD over BSON
   documents (with query options) and deliver field-level live deltas. The in-memory minimongo
   backend is here; a native Eio/libmongoc backend is a later addition behind the same signature. *)

type handle = { stop : unit -> unit }

(* A query is a selector plus the options that shape the result set. Defined here (not inside the
   signature) so it is ONE shared type across backends. *)
type query = {
  selector : Bson.t;
  sort : Bson.t;
  skip : int;
  limit : int;
  fields : Bson.t; (* projection spec *)
}

let query ?(selector = Bson.Document []) ?(sort = Bson.Document []) ?(skip = 0)
    ?(limit = 0) ?(fields = Bson.Document []) () =
  { selector; sort; skip; limit; fields }

module type S = sig
  type collection

  val insert : collection -> Bson.t -> string
  val update : collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
  val remove : collection -> Bson.t -> int
  val find : collection -> query -> Bson.t list
  val find_one : collection -> query -> Bson.t option
  val count : collection -> Bson.t -> int
  val aggregate : collection -> ?lookup:(string -> Bson.t list) -> Bson.t list -> Bson.t list
  val distinct : collection -> string -> Bson.t -> Bson.t list

  val observe_changes :
    collection ->
    query ->
    added:(string -> Bson.t -> unit) ->
    changed:(string -> Bson.t -> string list -> unit) ->
    removed:(string -> unit) ->
    handle

  (** [fence c k] runs [k] once every change committed to [c] {e so far} has been DELIVERED to its
      observers — the write-fence behind a method's [updated]. A backend whose deltas arrive over an
      external stream (a real mongod) may run [k] immediately (best-effort; documented there). *)
  val fence : collection -> (unit -> unit) -> unit

  (** Declare an index ([keys] = key spec, [unique] enforced); idempotent by [name]. Native →
      mongod createIndex; Mini → unique enforcement + name tracking (dev/test parity). *)
  val ensure_index : collection -> name:string -> keys:Bson.t -> unique:bool -> unit

  (** Drop an index by name (idempotent). *)
  val drop_index : collection -> name:string -> unit

  (** Existing index names — reconcile diffs against this (dropping only fennec-named orphans). *)
  val index_names : collection -> string list
end

(* The in-memory minimongo backend. *)
module Mini : S with type collection = Minimongo.t = struct
  type collection = Minimongo.t

  let cur c (q : query) =
    Minimongo.find c ~selector:q.selector ~sort:q.sort ~skip:q.skip ~limit:q.limit
      ~fields:q.fields ()

  let insert = Minimongo.insert
  let update c ~multi ~upsert sel m = Minimongo.update c ~multi ~upsert sel m
  let remove = Minimongo.remove
  let find c q = Minimongo.fetch (cur c q)
  let find_one c q = Minimongo.first (cur c q)
  let count c sel = Minimongo.count (Minimongo.find c ~selector:sel ())
  let aggregate c ?(lookup = fun _ -> []) pipeline = Minimongo.aggregate ~lookup c pipeline
  let distinct c key sel = Minimongo.distinct c ~key ~selector:sel ()

  let observe_changes c q ~added ~changed ~removed =
    (* the FULL query reaches minimongo: selector + projection on live deltas, and sort/skip/limit
       make it a WINDOWED observe whose window is maintained live (enter/displace/promote) — a
       limited publication never leaks past its limit *)
    let h =
      Minimongo.observe_changes
        (Minimongo.find c ~selector:q.selector ~sort:q.sort ~skip:q.skip ~limit:q.limit ~fields:q.fields ())
        ~added ~changed ~removed ()
    in
    { stop = h.Minimongo.stop }

  (* exact for the in-memory engine: the change stream IS the fanout being fenced *)
  let fence = Minimongo.on_drained

  let fields_of_keys = function Bson.Document kvs -> List.map fst kvs | _ -> []
  let ensure_index c ~name ~keys ~unique = Minimongo.ensure_index c ~name ~fields:(fields_of_keys keys) ~unique
  let drop_index c ~name = Minimongo.drop_index c ~name
  let index_names = Minimongo.index_names
end
