(** The canonical in-memory BSON value — shared by the query engine, Minimongo, and the native
    driver. Dependency-free (Stdlib only, no Yojson/Unix/Eio) so the same source compiles to native
    OCaml and, via js_of_ocaml, to JavaScript. An extended-JSON wire codec can be added as a
    separate, native-only module; this module provides only a debug rendering ({!to_string}).

    {[
      (* Build a selector / document with the constructor aliases, then read it back. *)
      let user = doc [ ("name", str "ada"); ("score", float 4.5); ("active", bool true) ] in
      get_string user "name"  (* Some "ada" *)
      |> ignore;
      get_float user "score"  (* Some 4.5 *)
      |> ignore;
      equal (int 1) (float 1.0)  (* true — numeric value equality *)
    ]} *)

(** A BSON value. Covers every BSON type; a [Document] is an {e ordered} field list (order is
    preserved on the wire and through projections, and {!equal} on documents is order-sensitive). *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | Document of (string * t) list  (** ordered fields, [name → value] *)
  | Array of t list
  | Object_id of string  (** a 24-character hex string *)
  | Date of int64  (** milliseconds since the Unix epoch *)
  | Timestamp of { t : int; i : int }  (** BSON timestamp: seconds [t] + ordinal [i] *)
  | Binary of { subtype : string; base64 : string }  (** [subtype] is 2 hex digits; payload base64 *)
  | Regex of { pattern : string; options : string }
  | Decimal128 of string  (** kept as its canonical decimal string form *)
  | Code of string  (** JavaScript code without scope *)
  | Code_with_scope of string * (string * t) list
  | Symbol of string
  | Min_key
  | Max_key

(** {2 Constructors} — small aliases for the common variants, so hand-built selectors and update
    documents read cleanly: [doc [ ("active", bool true); ("score", float 4.5) ]]. *)

(** [doc fields] is [Document fields]. *)
val doc : (string * t) list -> t

(** [str s] is [String s]. *)
val str : string -> t

(** [int n] is [Int n]. *)
val int : int -> t

(** [int64 n] is [Int64 n]. *)
val int64 : int64 -> t

(** [float f] is [Float f]. *)
val float : float -> t

(** [bool b] is [Bool b]. *)
val bool : bool -> t

(** [array xs] is [Array xs]. *)
val array : t list -> t

(** The [Null] value. *)
val null : t

(** [date ms] is [Date ms] (milliseconds since the epoch). *)
val date : int64 -> t

(** [oid s] is [Object_id s]; not validated — see {!object_id_of_string}. *)
val oid : string -> t

(** [object_id_of_string s] is [Some (Object_id s)] when [s] is exactly 24 hex characters, else
    [None] — the validating constructor for ObjectIds. *)
val object_id_of_string : string -> t option

(** [is_operator_key k] — whether a field key names a Mongo operator (starts with ['$']). *)
val is_operator_key : string -> bool

(** {2 Access} *)

(** [get t key] is the value of the top-level field [key] when [t] is a [Document], else [None]
    (also [None] for any non-document). It does {e not} interpret dotted paths — use
    {!Query.Matcher.get_path} for nested access. *)
val get : t -> string -> t option

(** [fields t] is the field list of a [Document] (or [[]] for any non-document). *)
val fields : t -> (string * t) list

(** Typed accessors: the top-level field as the requested kind, or [None] if absent or of a
    different kind. *)

(** The field as a [String]. *)
val get_string : t -> string -> string option

(** The field as an [Int] (also accepts [Int64]). *)
val get_int : t -> string -> int option

(** The field as a [Float] (also accepts [Int]/[Int64]). *)
val get_float : t -> string -> float option

(** The field as a [Bool]. *)
val get_bool : t -> string -> bool option

(** The field as an [Array]'s elements. *)
val get_list : t -> string -> t list option

(** [as_float v] is the value as a float when [v] is any numeric (or a [Date]), else [None] — the
    single "is this a number" test used across the engine. *)
val as_float : t -> float option

(** {2 Equality & ordering} *)

(** Value equality. The numeric types ([Int]/[Int64]/[Float]) compare by numeric {e value} (so
    [equal (int 1) (float 1.0)] is [true], as in MongoDB); [Document] equality is order-{e sensitive}
    on fields (also as in MongoDB); [Float nan] equals nothing, including itself. *)
val equal : t -> t -> bool

(** A {e total} order over all values, suitable for sorting. Different BSON types are ordered by a
    fixed type precedence (mirroring MongoDB's), never by constructor declaration order; numbers
    compare by value with [nan] sorted lowest. (For range {e queries}, the matcher instead uses
    same-type comparison — a number query never matches a string.) *)
val compare : t -> t -> int

(** {2 Debug rendering} *)

(** A compact, human-readable rendering for logs, assertions, and the REPL. This is {e not}
    canonical extended-JSON — it is for debugging only (e.g. [{a: 1, b: "x"}]). *)
val to_string : t -> string

(** [pp] formats via {!to_string}. *)
val pp : Format.formatter -> t -> unit
