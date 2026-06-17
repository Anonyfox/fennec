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
    Pure, bson-only, js_of_ocaml-safe — shared verbatim by server and browser.

    {[ type t = { id : string; title : string }
       let codec =
         Sift.(record (fun id title -> { id; title })
                |> field doc_id (fun t -> t.id)
                |> field (req "title" (non_empty (max_len 200 string))) (fun t -> t.title)
                |> seal)
       let bson = Sift.encode_checked codec { id = ""; title = "Buy milk" }   (* validates, then encodes *) ]}

    In practice [@@deriving collection] generates this codec (plus the typed [Fields] and the
    collection) straight from the record — write the type, not the builder. *)

(** {1 Errors} *)

(** One violation. [path] is where (field path, outermost first); [msg] is the human (English-default)
    message; [code] is a machine-readable rule id (["required"], ["type"], ["min_len"], ["max_len"],
    ["pattern"], ["one_of"], ["min"], ["max"], ["finite"], ["check"], …) and [params] its
    interpolation values (e.g. [[("n", "80")]]) — together they let a translator localize without
    re-parsing [msg]. Decoding/validation COLLECTS every violation (forms need the full list). *)
type error = { path : string list; msg : string; code : string; params : (string * string) list }

(** Build an error (the [code]/[params] default to empty) — for code producing its own violations. *)
val err : ?code:string -> ?params:(string * string) list -> string list -> string -> error

(** Re-render an error's [msg] through a translator (which reads its [code]/[params]/[path]) — the
    i18n hook: map a list of errors through [translate tr] before rendering/serializing. *)
val translate : (error -> string) -> error -> error

val error_to_string : error -> string
val errors_to_string : error list -> string

(** {1 The codec value} *)

(** The type representation — abstract here; reflect with {!view}. *)
type 'a shape

(** A codec: the representation plus the precompiled encode/decode. [dec]'s error is the RENDERED
    collected errors (back-compat); use {!decode} for the structured list. [enc] is total — a typed
    value always serializes; refinement checking on the write side is {!validate}/{!encode_checked}. *)
type 'a t = { shape : 'a shape; enc : 'a -> Bson.t; dec : Bson.t -> ('a, string) result }

(** Structured decode: every violation, each with its field path. *)
val decode : 'a t -> Bson.t -> ('a, error list) result

(** Decode a top-level BSON document {e buffer} into the value, schema-directed and single-pass,
    WITHOUT building a {!Bson.t} tree — scans each document level into a flat span index and reads
    only the wanted fields straight from the bytes (unwanted fields are skipped by their length
    prefix; keys match in place; strings copy only when an owned result is demanded). Returns the
    SAME value and SAME path-tagged errors as {!decode}; a malformed buffer is a structured error,
    never an exception. The fast path for decoding stored/wire documents. *)
val decode_bytes : 'a t -> Bigstringaf.t -> ('a, error list) result

(** Decode ONE top-level field of a BSON document buffer, by key, reading only that field's bytes —
    the rest of the document is skipped by length prefix, never materialized. [None] when the field is
    absent; [Some (Ok v)] / [Some (Error _)] when present (value decoded and its checks run; errors
    path-tagged with the key). The projection primitive — route a raw document by its [_id] or a
    variant discriminator, or pull a single value, without decoding the whole record. *)
val peek : 'a t -> string -> Bigstringaf.t -> ('a, error list) result option

(** Validate a BSON document buffer against the codec WITHOUT materializing the value — the alloc-free
    tier. Returns the SAME Ok/Error verdict (and errors) as {!decode_bytes}, but runs at scan speed with
    ~0 allocation when the shape's checks are all structural (types, required, nesting). For shapes with
    refinements or cross-field rules — which need the materialized value — it falls back to a full decode
    and discards the value. The in-process [$jsonSchema]: gate a wire/storage write without decoding it. *)
val valid_bytes : 'a t -> Bigstringaf.t -> (unit, error list) result

(** Is [buf] a structurally well-formed BSON document (consistent lengths, known type tags, terminated
    keys, sound nesting, no out-of-bounds)? Shape-agnostic, single pass, ZERO allocation — a fast
    pre-filter / fuzz oracle, the pure-OCaml analog of libbson's [bson_validate]. *)
val scan_valid : Bigstringaf.t -> bool

(** {1 Encode (zero-copy, SIFT-K3)} *)

(** The EXACT wire byte length of encoding [v] through the codec, WITHOUT building it — equals
    [String.length] of the BSON the codec would emit. For pre-sizing a buffer, a Content-Length header,
    a quota check; and the basis of straight-to-buffer encode ({!encode_bytes}). *)
val size : 'a t -> 'a -> int

(** Encode [v] straight into a freshly-allocated buffer ([Bigstringaf.create (size c v)]) in a single
    pass, WITHOUT building a {!Bson.t} tree — byte-identical to encoding through the tree then
    serialising to the wire. Each document's length prefix is backpatched once its content end is known. *)
val encode_bytes : 'a t -> 'a -> Bigstringaf.t

(** {1 Derived operations (SIFT-K6)} *)

(** Structural equality derived from the shape — monomorphic (not OCaml's polymorphic [=]): a record
    is equal field-by-field, a variant when same-case with equal bodies. *)
val equal : 'a t -> 'a -> 'a -> bool

(** Total ordering derived from the shape (lexicographic over fields / list elements; variants by case
    declaration order). Monomorphic. *)
val compare : 'a t -> 'a -> 'a -> int

(** A structural hash derived from the shape (monomorphic; combines field hashes). *)
val hash : 'a t -> 'a -> int

(** A sensible default/zero value from the shape: leaves zero out (["" / 0 / false / []]); a record is
    built from its fields' defaults (required fields get their leaf default); a variant takes its first
    case. For form initial values and fixtures. *)
val default : 'a t -> 'a

(** The minimal change from [old] to [new] for a document-shaped codec (record / map / variant), as a
    Mongo-style update: [set] are the fields that changed or appeared (with their new encoded value),
    [unset] are the fields that became absent (an optional gone to [None], an opt_list to [[]]). Maps
    straight onto a Mongo [$set]/[$unset] modifier and a DDP [changed]/[cleared] message — the
    reactive-sync / minimal-update primitive. *)
type delta = { set : (string * Bson.t) list; unset : string list }

val diff : 'a t -> 'a -> 'a -> delta

(** Run every check against an in-memory value — the encode-side gate (writes validate), and the
    form-feedback primitive (same checks, synchronously, offline-capable). *)
val validate : 'a t -> 'a -> (unit, error list) result

(** {!validate} then [enc] — the one-call write boundary. *)
val encode_checked : 'a t -> 'a -> (Bson.t, error list) result

(** Derived pretty-printing — nested documents, lists, options, variants, all readable. *)
val pp : 'a t -> Format.formatter -> 'a -> unit

val show : 'a t -> 'a -> string

(** {1 JSON I/O}

    The shape is described once; {!encode_checked}/{!decode} speak BSON (the storage form), these speak
    JSON (the wire form for APIs) via the extended-JSON bridge — so DB shape = wire shape, no second
    serializer. Encode is total (mirrors the BSON encode); decode validates with the same
    path-collected errors as {!decode}.

    {[ let body = Sift.to_json_string Post.codec post   (* a JSON API response *)
       match Sift.of_json_string Post.codec request_body with Ok p -> … | Error es -> … ]} *)

(** Extended-JSON AST (re-exported), for composing with other JSON code. *)
module Json = Fennec_mongo_json.Json

(** Encode a value to a JSON AST (total). *)
val to_json : 'a t -> 'a -> Json.t

(** Decode + validate a value from a JSON AST. *)
val of_json : 'a t -> Json.t -> ('a, error list) result

(** Encode a value to a JSON string (total). *)
val to_json_string : 'a t -> 'a -> string

(** Decode + validate a value from a JSON string ([malformed JSON] error on a parse failure). *)
val of_json_string : 'a t -> string -> ('a, error list) result

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

(** [map to_ of_ c] — a total two-way transform of [c]'s decoded value ({!conv} without the fallible
    decode side). *)
val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** Make decode ALSO accept a [String] and parse it to the leaf — for numbers / bools / dates that
    arrive stringly (HTML forms, query strings, lenient JSON). A native value still decodes directly,
    encoding is unchanged, and it reflects as the inner type (so schema / form rendering are unaffected).
    {[ Sift.(from_string (min_i 1 int)) (* accepts 5 or "5" *) ]} *)
val from_string : 'a t -> 'a t

(** A self-referential codec — trees, comment threads, nested menus. The [self] handed to [f] refers
    back to the whole codec and is forced lazily, so finite values terminate.
    {[ let rec tree = Sift.fix (fun tree ->
         seal (record (fun v kids -> { v; kids })
               |> field (req "v" string) (fun t -> t.v)
               |> field (opt_list "kids" tree) (fun t -> t.kids))) ]}
    Reflects as opaque ([V_bson]) — a finite {!view} can't unfold a cycle. *)
val fix : ('a t -> 'a t) -> 'a t

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
         Sift.(record (fun id title tags -> { id; title; tags })
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

(** [dot outer inner] navigates into an embedded record for a selector/modifier PATH: the result
    has the dotted wire name (e.g. ["author.name"]) and the LEAF's shape — so
    [Filter.eq (Sift.dot Fields.author Author.Fields.name) v] compile-checks both field names AND the
    value's type, purely value-level. Chains: [dot a (dot b c)] → ["a.b.c"]. *)
val dot : _ field -> 'a field -> 'a field

(** {2 Field accessors — what the collection vocabulary (Filter/M/Index) builds on} *)

val field_name : 'a field -> string

(** Encode a value as this field stores it (checks/norms/convs respected). *)
val field_enc : 'a field -> 'a -> Bson.t

(** Encode ONE element of a list field (for [$push]-style modifiers). *)
val field_elem_enc : 'a list field -> 'a -> Bson.t

(** Run the field's own checks against a value (errors carry the field name). *)
val field_validate : 'a field -> 'a -> (unit, error list) result

(** Decode ONE field out of a document (raw decode + the field's checks) — the projection
    primitive: read a projected slice without ever constructing the full record. *)
val field_get : 'a field -> Bson.t -> ('a, error list) result

type ('r, 'k) builder

val record : 'k -> ('r, 'k) builder
val field : 'a field -> ('r -> 'a) -> ('r, 'a -> 'k) builder -> ('r, 'k) builder

(** A record-level (cross-field) check, e.g. [checking (fun t -> t.starts < t.ends) "starts must precede ends"]. *)
val checking : ('r -> bool) -> string -> ('r, 'k) builder -> ('r, 'k) builder

val seal : ('r, 'r) builder -> 'r t

(** {1 Variants — tagged unions over a discriminator field (Mongo's polymorphic-document idiom)}

    {[ type shape = Circle of { r : float } | Rect of { w : float; h : float }
       let codec = Sift.(variant ~tag:"kind"
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

(** The structural {!view} of one field's shape — a form renderer reads the leaf kind + refinement
    {!hint}s from it to infer the input type and emit HTML5 constraint attributes
    ([required]/[maxlength]/[min]/[pattern]…) without touching the GADT. *)
val field_view : 'a field -> view

(** Whether this field is required (drives the [required] attribute). *)
val field_required : 'a field -> bool

(** {1 Positional parameter lists (DDP method params)} *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

val a0 : unit args
val a1 : 'a t -> 'a args
val a2 : 'a t -> 'b t -> ('a * 'b) args
val a3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) args
