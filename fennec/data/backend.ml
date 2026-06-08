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
  val aggregate : collection -> Bson.t list -> Bson.t list
  val distinct : collection -> string -> Bson.t -> Bson.t list

  val observe_changes :
    collection ->
    query ->
    added:(string -> Bson.t -> unit) ->
    changed:(string -> Bson.t -> string list -> unit) ->
    removed:(string -> unit) ->
    handle
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
  let aggregate c pipeline = Minimongo.aggregate c pipeline
  let distinct c key sel = Minimongo.distinct c ~key ~selector:sel ()

  let observe_changes c q ~added ~changed ~removed =
    (* projection is honoured on live deltas; sort/skip/limit shape the snapshot *)
    let h =
      Minimongo.observe_changes
        (Minimongo.find c ~selector:q.selector ~fields:q.fields ())
        ~added ~changed ~removed ()
    in
    { stop = h.Minimongo.stop }
end
