(** The canonical in-memory BSON value — shared by the query engine, Minimongo, and the native
    driver. Dependency-free (Stdlib only, no Yojson/Unix/Eio) so the same source compiles to native
    OCaml and, via js_of_ocaml, to JavaScript. The extended-JSON wire codec is a separate,
    native-only module. *)

(** A BSON value. Covers every BSON type; a [Document] is an {e ordered} field list (order is
    preserved on the wire and through projections). *)
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

(** [doc fields] is [Document fields] — a brevity alias. *)
val doc : (string * t) list -> t

(** [str s] is [String s]. *)
val str : string -> t

(** [int n] is [Int n]. *)
val int : int -> t

(** [oid s] is [Object_id s] (a 24-char hex string; not validated here). *)
val oid : string -> t

(** [get t key] is the value of the top-level field [key] when [t] is a [Document], else [None]
    (also [None] for any non-document). It does {e not} interpret dotted paths — use
    {!Query.Matcher.get_path} for nested access. *)
val get : t -> string -> t option
