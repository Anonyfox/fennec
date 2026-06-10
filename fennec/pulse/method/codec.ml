(* Bson codecs — the typed edge of the method layer. A ['a t] encodes/decodes ONE value; an
   ['a args] maps a whole parameter LIST (DDP method params are positional). Decoding doubles as
   validation: the server turns a [dec] failure into a 400 before the handler ever runs, which
   subsumes Meteor's check/ValidatedMethod. Value-level combinators on purpose — no ppx, no functors,
   so hundreds of methods stay cheap to compile. *)

type 'a t = { enc : 'a -> Bson.t; dec : Bson.t -> ('a, string) result }

let make ~enc ~dec = { enc; dec }

(* a short constructor tag for error messages *)
let kind : Bson.t -> string = function
  | Bson.Null -> "null"
  | Bson.Bool _ -> "bool"
  | Bson.Int _ -> "int"
  | Bson.Int64 _ -> "int64"
  | Bson.Float _ -> "float"
  | Bson.String _ -> "string"
  | Bson.Document _ -> "document"
  | Bson.Array _ -> "array"
  | Bson.Object_id _ -> "objectid"
  | Bson.Date _ -> "date"
  | _ -> "value"

let expected what v = Error (Printf.sprintf "expected %s, got %s" what (kind v))

let string = { enc = (fun s -> Bson.String s); dec = (function Bson.String s -> Ok s | v -> expected "string" v) }

let int =
  { enc = (fun n -> Bson.Int n);
    dec =
      (function
      | Bson.Int n -> Ok n
      | Bson.Float f when Float.is_integer f -> Ok (int_of_float f) (* EJSON numbers arrive as floats *)
      | v -> expected "int" v) }

let float =
  { enc = (fun f -> Bson.Float f);
    dec = (function Bson.Float f -> Ok f | Bson.Int n -> Ok (float_of_int n) | v -> expected "float" v) }

let bool = { enc = (fun b -> Bson.Bool b); dec = (function Bson.Bool b -> Ok b | v -> expected "bool" v) }

(* the raw escape hatch: any Bson value passes through untyped *)
let bson = { enc = Fun.id; dec = (fun v -> Ok v) }

let unit =
  { enc = (fun () -> Bson.Null); dec = (function Bson.Null -> Ok () | v -> expected "null" v) }

let option c =
  { enc = (function Some v -> c.enc v | None -> Bson.Null);
    dec = (function Bson.Null -> Ok None | v -> Result.map Option.some (c.dec v)) }

let list c =
  { enc = (fun xs -> Bson.Array (List.map c.enc xs));
    dec =
      (function
      | Bson.Array xs ->
          let rec go acc = function
            | [] -> Ok (List.rev acc)
            | x :: tl -> ( match c.dec x with Ok v -> go (v :: acc) tl | Error e -> Error e)
          in
          go [] xs
      | v -> expected "array" v) }

(* [conv dec enc base] builds a codec for a richer type over [base] — [dec] may reject. *)
let conv (dec : 'a -> ('b, string) result) (enc : 'b -> 'a) (base : 'a t) : 'b t =
  { enc = (fun b -> base.enc (enc b)); dec = (fun v -> Result.bind (base.dec v) dec) }

(* [field d name c] reads field [name] of document [d] with [c] — for [conv]-built record codecs. *)
let field (d : Bson.t) (name : string) (c : 'a t) : ('a, string) result =
  match d with
  | Bson.Document kvs -> (
      match List.assoc_opt name kvs with
      | Some v -> ( match c.dec v with Ok a -> Ok a | Error e -> Error (name ^ ": " ^ e))
      | None -> Error ("missing field " ^ name))
  | v -> expected "document" v

(* ---- positional parameter lists (DDP method params) ---- *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

let arity n got = Error (Printf.sprintf "expected %d argument(s), got %d" n (List.length got))

let a0 : unit args =
  { enc_args = (fun () -> []); dec_args = (function [] -> Ok () | l -> arity 0 l) }

let a1 (c : 'a t) : 'a args =
  { enc_args = (fun a -> [ c.enc a ]); dec_args = (function [ x ] -> c.dec x | l -> arity 1 l) }

let a2 (ca : 'a t) (cb : 'b t) : ('a * 'b) args =
  { enc_args = (fun (a, b) -> [ ca.enc a; cb.enc b ]);
    dec_args =
      (function
      | [ x; y ] -> Result.bind (ca.dec x) (fun a -> Result.map (fun b -> (a, b)) (cb.dec y))
      | l -> arity 2 l) }

let a3 (ca : 'a t) (cb : 'b t) (cc : 'c t) : ('a * 'b * 'c) args =
  { enc_args = (fun (a, b, c) -> [ ca.enc a; cb.enc b; cc.enc c ]);
    dec_args =
      (function
      | [ x; y; z ] ->
          Result.bind (ca.dec x) (fun a ->
              Result.bind (cb.dec y) (fun b -> Result.map (fun c -> (a, b, c)) (cc.dec z)))
      | l -> arity 3 l) }
