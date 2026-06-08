(* A friendly BSON value type — the canonical in-memory value shared by the query engine,
   Minimongo, and (later) the native driver. This module is deliberately dependency-free (no
   Yojson, no Unix, no Eio) so the SAME source compiles to native OCaml and, via js_of_ocaml, to
   JavaScript. The extended-JSON codec lives in a separate native-only module; a JS build supplies
   its own JS-object codec instead. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | Document of (string * t) list
  | Array of t list
  | Object_id of string (* 24-char hex *)
  | Date of int64 (* milliseconds since the Unix epoch *)
  | Timestamp of { t : int; i : int } (* BSON timestamp: seconds + ordinal *)
  | Binary of { subtype : string; base64 : string } (* subtype is 2-hex *)
  | Regex of { pattern : string; options : string }
  | Decimal128 of string (* decimal kept as its canonical string form *)
  | Code of string (* JavaScript code without scope *)
  | Code_with_scope of string * (string * t) list
  | Symbol of string
  | Min_key
  | Max_key

(* ---- construction helpers ------------------------------------------------ *)

let doc fields = Document fields
let str s = String s
let int n = Int n
let oid s = Object_id s

let get t key =
  match t with Document kvs -> List.assoc_opt key kvs | _ -> None
