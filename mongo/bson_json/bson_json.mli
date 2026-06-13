(** Extended-JSON codec for {!Bson.t} — the bridge to the C driver (libmongoc speaks MongoDB
    canonical/relaxed extended JSON). {!to_string} emits {e canonical} extended JSON: every typed
    number is wrapped as a string ([$numberInt]/[$numberLong]/[$numberDouble]), so precision survives
    regardless of the JSON number representation, and Decimal128 is just [{$numberDecimal: "..."}] (a
    string libbson interprets — no decimal128 binary math here). {!of_string} reads both canonical
    and relaxed forms back; anything unmodelled degrades to a plain document rather than being
    dropped. Pure ({!Bson} + the fennec-mongo JSON primitive) — native and browser.

    {[
      (* Round-trip a BSON value through the wire format the C driver speaks. *)
      let doc = Bson.doc [ ("n", Bson.int 42); ("when", Bson.Date 1700000000000L) ] in
      let wire = to_string doc in       (* canonical: {"n":{"$numberInt":"42"},…} *)
      let back = of_string wire in       (* reads canonical or relaxed *)
      Bson.equal doc back                (* true *)
    ]} *)

(** [to_json b] is the canonical extended-JSON AST of [b]. *)
val to_json : Bson.t -> Fennec_mongo_json.Json.t

(** [of_json j] reads a (canonical or relaxed) extended-JSON AST back into a {!Bson.t}. *)
val of_json : Fennec_mongo_json.Json.t -> Bson.t

(** [to_string b] is the canonical extended-JSON string of [b] — what the driver sends to libmongoc. *)
val to_string : Bson.t -> string

(** [of_string s] parses an extended-JSON string into a {!Bson.t}.
    @raise Fennec_mongo_json.Json.Parse_error on malformed JSON. *)
val of_string : string -> Bson.t

(** [of_string_opt s] is {!of_string} returning [None] instead of raising on malformed input. *)
val of_string_opt : string -> Bson.t option

(** [list_of_string s] reads a JSON array of documents (e.g. a driver find result) into a list; a
    non-array value degrades to a singleton list. *)
val list_of_string : string -> Bson.t list
