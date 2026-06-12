(** The shape language — Pulse's foundation for saying what a value IS.

    One GADT type representation inside; plain combinators outside. From one declaration the
    framework derives: the codec (validating decode with path-collected errors), encode-side
    validation ({!validate} / {!encode_checked} — an invalid value cannot pass a write boundary),
    normalizers, derived pretty-printing ({!pp}/{!show}, nested), and the neutral {!view}
    reflection downstream renderers consume ($jsonSchema, OpenAPI, the admin UI) without this
    module knowing them.

    Refinements carry a machine-readable {!hint} (what a schema renderer can translate); an
    arbitrary {!check} is honestly app-side-only. Floats reject nan/inf by default. Options:
    absent OR null decode to [None]; [None] encodes by OMITTING the key (Mongo-idiomatic).
    Pure, bson-only, js_of_ocaml-safe — shared verbatim by server and browser. *)

(** {1 Errors} *)

(** One violation: where (field path, outermost first) and why. Decoding/validation COLLECTS every
    violation — forms need the full list, not first-fail. *)
type error = { path : string list; msg : string }

val error_to_string : error -> string
val errors_to_string : error list -> string

(** {1 The codec value} *)

(** The type representation — abstract here; reflect with {!view}. *)
type 'a ty

(** A codec: the representation plus the precompiled encode/decode. [dec]'s error is the RENDERED
    collected errors (back-compat); use {!decode} for the structured list. [enc] is total — a typed
    value always serializes; refinement checking on the write side is {!validate}/{!encode_checked}. *)
type 'a t = { ty : 'a ty; enc : 'a -> Bson.t; dec : Bson.t -> ('a, string) result }

(** Structured decode: every violation, each with its field path. *)
val decode : 'a t -> Bson.t -> ('a, error list) result

(** Run every check against an in-memory value — the encode-side gate (writes validate), and the
    form-feedback primitive (same checks, synchronously, offline-capable). *)
val validate : 'a t -> 'a -> (unit, error list) result

(** {!validate} then [enc] — the one-call write boundary. *)
val encode_checked : 'a t -> 'a -> (Bson.t, error list) result

(** Derived pretty-printing — nested documents, lists, options, variants, all readable. *)
val pp : 'a t -> Format.formatter -> 'a -> unit

val show : 'a t -> 'a -> string

(** {1 Primitives} *)

val string : string t
val int : int t (* accepts an integral Float (EJSON reality) *)

val float : float t
(** nan/inf are REJECTED (decode and validate) — silently storing them is how data rots. *)

val float_nonfinite : float t
(** The opt-out: a float that may carry nan/inf. *)

val bool : bool t
val date : int64 t (* Bson.Date, ms since epoch (also accepts integral numbers) *)

val id : string t
(** An [_id] value: accepts [String] or [ObjectId] (surfaced as the hex string); encodes as
    [ObjectId] when the value looks like one (24 hex chars), [String] otherwise. *)

val bson : Bson.t t (* the dynamic escape hatch *)
val unit : unit t
val list : 'a t -> 'a list t
val option : 'a t -> 'a option t

val str_map : 'a t -> (string * 'a) list t
(** A dynamic-key subdocument (Mongo dictionaries): each value checked by the element codec. *)

(** [conv proj inj c] maps a codec to another type ([proj] may reject). *)
val conv : ('a -> ('b, string) result) -> ('b -> 'a) -> 'a t -> 'b t

(** An arbitrary codec from closures (reflects as opaque [V_bson]). *)
val make : enc:('a -> Bson.t) -> dec:(Bson.t -> ('a, string) result) -> 'a t

(** {1 Refinements and normalizers}

    Stackable — wrap as many as needed; violations collect. Named refinements carry a {!hint} a
    schema renderer translates (so mongod enforces them against foreign writers too); {!check} is
    arbitrary and app-side-only. Normalizers run BEFORE checks, on decode and encode alike. *)

val check : ?msg:string -> ('a -> bool) -> 'a t -> 'a t

val min_len : int -> string t -> string t
val max_len : int -> string t -> string t
val non_empty : string t -> string t

val pattern : string -> string t -> string t
(** A deliberately PORTABLE matcher — anchors, classes, [+ * ? .], literals — the common subset
    that means the same here, in the browser, and in mongod's $jsonSchema dialect. *)

val one_of : string list -> string t -> string t
val email : string t -> string t
val url : string t -> string t
val slug : string t -> string t
val trim : string t -> string t
val lowercase : string t -> string t
val min_i : int -> int t -> int t
val max_i : int -> int t -> int t
val positive_i : int t -> int t
val min_f : float -> float t -> float t
val max_f : float -> float t -> float t
val positive : float t -> float t
val non_negative : float t -> float t
val multiple_of : float -> float t -> float t
val min_items : int -> 'a list t -> 'a list t
val max_items : int -> 'a list t -> 'a list t
val unique_items : 'a list t -> 'a list t

(** {1 Records — the builder (what the [@@fennec.collection] deriver targets)}

    {[ type t = { id : string; title : string; tags : string list }
       let codec =
         Codec.(record (fun id title tags -> { id; title; tags })
                |> field doc_id (fun t -> t.id)
                |> field (req "title" (min_len 3 string)) (fun t -> t.title)
                |> field (opt_list "tags" string) (fun t -> t.tags)
                |> seal) ]} *)

(** One declared field: wire name + shape (+ requiredness / default). *)
type 'a field

(** Required: missing or malformed collects an error naming the field. *)
val req : string -> 'a t -> 'a field

(** Optional: absent or [Null] → [None]; [None] encodes by omitting the key. *)
val opt : string -> 'a t -> 'a option field

(** A list that tolerates absence: absent → [[]]; [[]] encodes by omitting the key. *)
val opt_list : string -> 'a t -> 'a list field

(** Required-with-default: absent decodes to the default. *)
val dft : string -> 'a t -> 'a -> 'a field

(** The ["_id"] field ([req "_id" id]). *)
val doc_id : string field

(** {2 Field accessors — what the collection vocabulary (Q/M/Index) builds on} *)

val field_name : 'a field -> string

(** Encode a value as this field stores it (checks/norms/convs respected). *)
val field_enc : 'a field -> 'a -> Bson.t

(** Encode ONE element of a list field (for [$push]-style modifiers). *)
val field_elem_enc : 'a list field -> 'a -> Bson.t

(** Run the field's own checks against a value (errors carry the field name). *)
val field_validate : 'a field -> 'a -> (unit, error list) result

type ('r, 'k) builder

val record : 'k -> ('r, 'k) builder
val field : 'a field -> ('r -> 'a) -> ('r, 'a -> 'k) builder -> ('r, 'k) builder

(** A record-level (cross-field) check, e.g. [checking (fun t -> t.starts < t.ends) "starts must precede ends"]. *)
val checking : ('r -> bool) -> string -> ('r, 'k) builder -> ('r, 'k) builder

val seal : ('r, 'r) builder -> 'r t

(** {1 Variants — tagged unions over a discriminator field (Mongo's polymorphic-document idiom)}

    {[ type shape = Circle of { r : float } | Rect of { w : float; h : float }
       let codec = Codec.(variant ~tag:"kind"
         [ case "circle" (record (fun r -> Circle { r }) |> field (req "r" float) (function Circle c -> c.r | _ -> 0.))
             ~inj:Fun.id ~proj:(function Circle _ as v -> Some v | _ -> None); ... ]) ]}

    Decode reads the tag and dispatches; encode writes the tag plus the case's fields. Exhaustive
    matching on the OCaml side is the point. *)

type 'r vcase

val case : string -> ('a, 'a) builder -> inj:('a -> 'r) -> proj:('r -> 'a option) -> 'r vcase
val variant : tag:string -> 'r vcase list -> 'r t

(** {1 Tuple-style records (back-compat; prefer the builder)} *)

val obj1 : 'a field -> make:('a -> 'r) -> split:('r -> 'a) -> 'r t
val obj2 : 'a field -> 'b field -> make:('a -> 'b -> 'r) -> split:('r -> 'a * 'b) -> 'r t

val obj3 :
  'a field -> 'b field -> 'c field -> make:('a -> 'b -> 'c -> 'r) -> split:('r -> 'a * 'b * 'c) -> 'r t

val obj4 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  make:('a -> 'b -> 'c -> 'd -> 'r) ->
  split:('r -> 'a * 'b * 'c * 'd) ->
  'r t

(** {1 Introspection — the neutral reflection renderers consume}

    Pure data: a schema renderer (the collection lib's $jsonSchema), OpenAPI, or an admin UI walks
    this without the GADT ever crossing the library boundary. *)

(** The renderable half of a refinement. [H_none] = an arbitrary check (app-side-only, honestly). *)
type hint =
  | H_none
  | H_min_len of int
  | H_max_len of int
  | H_pattern of string
  | H_enum of string list
  | H_min of float
  | H_max of float
  | H_multiple_of of float
  | H_min_items of int
  | H_max_items of int
  | H_unique_items

type view =
  | V_string
  | V_int
  | V_float
  | V_bool
  | V_date
  | V_id
  | V_bson
  | V_unit
  | V_list of view
  | V_option of view
  | V_map of view
  | V_check of hint * view
  | V_obj of (string * bool * view) list (* name, required, shape *)
  | V_variant of string * (string * (string * bool * view) list) list (* tag, cases *)

val view : 'a t -> view

(** {1 Positional parameter lists (DDP method params)} *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

val a0 : unit args
val a1 : 'a t -> 'a args
val a2 : 'a t -> 'b t -> ('a * 'b) args
val a3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) args
