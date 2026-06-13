(** Id generation. By default, 17-character "unmistakable character" string ids (Meteor's default —
    no easily-confused glyphs); also 24-hex ObjectIds. The randomness source is a parameter
    ([rng n] returns an int in [0, n)), so tests can inject a deterministic stream and a JS build
    can plug in [Math.random]; the logic itself stays pure.

    {[
      (* the default id generator a Minimongo collection mints _id with *)
      let _id = Id.random_id () in              (* e.g. "Lp4mK2..." (17 chars) *)
      let oid = Id.object_id () in              (* 24 lowercase-hex chars *)
      (* deterministic stream for tests *)
      let seq = ref 0 in
      let fixed = Id.random_id ~rng:(fun n -> incr seq; !seq mod n) ()
    ]} *)

(** [random_id ?n ?rng ()] — an [n]-character id (default 17) drawn from the unmistakable alphabet.
    [rng] defaults to {!Stdlib.Random}. *)
val random_id : ?n:int -> ?rng:(int -> int) -> unit -> string

(** [object_id ?rng ()] — a 24-character lowercase-hex id. *)
val object_id : ?rng:(int -> int) -> unit -> string
