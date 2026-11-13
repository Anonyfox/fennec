(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary type for reporting syntax errors

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** A syntax error, with the locations needed to report it. *)
type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * string
  | Removed_string_set of Location.t

(** Raised by the parser on a syntax error. *)
exception Error of error

(** Raised internally to escape from the parser on an error without details. *)
exception Escape_error

(** The main location of a syntax error. *)
val location_of_error: error -> Location.t

(** Raise an [Ill_formed_ast] error at the location with the given message. *)
val ill_formed_ast: Location.t -> string -> 'a
