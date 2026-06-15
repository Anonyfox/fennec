(* GridFS — store files (any size, past the 16 MB document limit) as standard MongoDB GridFS: a metadata
   document in [<bucket>.files] plus the bytes split into 255 KiB [<bucket>.chunks], wire-compatible
   with mongod's GridFS (same field names/types: length / chunkSize / uploadDate / files_id / n / data).

   It is a functor over a minimal insert/find/remove/ensure_index store — nothing engine-specific — so
   the SAME code runs over minimongo, Burrow, or a real mongod. Pure (only Bson + an inline base64 +
   ObjectId minting), so instantiated over minimongo it cross-compiles to JS: file METADATA can sync to
   the browser like any reactive collection, while the bytes are served from the server's store (you
   normally would not pump multi-MB chunks through a browser's minimongo — serve them over HTTP). The
   clock is injected ([~now]) so the core stays free of Unix/Date. *)

module type STORE = sig
  type collection

  val insert : collection -> Bson.t -> unit
  val find : collection -> selector:Bson.t -> sort:Bson.t -> Bson.t list
  val remove : collection -> Bson.t -> int
  val ensure_index : collection -> name:string -> keys:Bson.t -> unique:bool -> unit
end

val default_chunk_size : int
(** 261120 = 255 KiB — the GridFS default chunk size. *)

(** Base64 used for the chunk payloads ([Bson.Binary] holds base64); exposed for testing. *)
val base64_encode : string -> string

val base64_decode : string -> string

module Make (S : STORE) : sig
  type bucket

  val bucket : ?chunk_size:int -> files:S.collection -> chunks:S.collection -> unit -> bucket
  (** A GridFS bucket over a [files] and a [chunks] collection. Ensures the unique
      [{files_id:1, n:1}] index on [chunks] (the standard GridFS index). *)

  val upload : bucket -> now:int64 -> ?filename:string -> ?metadata:Bson.t -> string -> string
  (** [upload b ~now ?filename ?metadata bytes] stores [bytes] as a new file (chunks first, then the
      metadata doc, so a file only becomes visible once whole) and returns its id (24-hex ObjectId).
      [now] is the upload time in ms since the epoch (injected — the core takes no clock). *)

  val download : bucket -> string -> string option
  (** Reassemble a file's bytes by id (chunks ordered by [n]); [None] if no such file. *)

  val stat : bucket -> string -> Bson.t option
  (** The file's [<bucket>.files] metadata document by id. *)

  val list : bucket -> Bson.t list
  (** All file metadata documents in the bucket. *)

  val delete : bucket -> string -> unit
  (** Remove a file's metadata document and all its chunks. *)
end
