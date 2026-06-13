(** Pure document helpers + the diff/LCS machinery shared by the observe engines. No I/O — cross-
    compiles to JavaScript unchanged.

    {[
      (* how an observeChanges engine classifies one mutation and emits the right callback *)
      match Diff.transition ~was ~now with
      | Diff.Entered -> added id (Diff.fields_without_id new_doc)
      | Diff.Left -> removed id
      | Diff.Stayed ->
          let changed_fields, cleared = Diff.diff_fields ~old_doc ~new_doc in
          if changed_fields <> [] || cleared <> [] then changed id changed_fields cleared
      | Diff.Outside -> ()
    ]} *)

(** The field list of a [Document] (or [[]] for any non-document). *)
val kvs_of : Bson.t -> (string * Bson.t) list

(** Render an id value ([String]/[Object_id]/[Int]/[Int64]) to its string form; [""] otherwise. *)
val id_to_string : Bson.t -> string

(** The [_id] of a document, as a string (Minimongo ids are strings by default); [""] if absent. *)
val doc_id : Bson.t -> string

(** A copy of the document with its [_id] field removed. *)
val fields_without_id : Bson.t -> Bson.t

(** [merge_doc base ~updated ~removed] applies a partial update onto [base]: set/replace the
    [updated] fields and drop the [removed] field names. Top-level keys only; field order is
    preserved for stable output. *)
val merge_doc : Bson.t -> updated:(string * Bson.t) list -> removed:string list -> Bson.t

(** [diff_fields ~old_doc ~new_doc] returns [(changed, cleared)]: the fields set or added going
    old→new, and the names of fields that vanished. The [_id] field is ignored. *)
val diff_fields : old_doc:Bson.t -> new_doc:Bson.t -> (string * Bson.t) list * string list

(** A document's membership transition across one mutation (was it in the result set, is it now). *)
type transition = Entered | Stayed | Left | Outside

(** [transition ~was ~now] classifies a membership change. *)
val transition : was:bool -> now:bool -> transition

(** [diff_ordered ~old_list ~new_list ~added_before ~changed ~moved_before ~removed] emits the
    ordered observeChanges operations that transform [old_list] into [new_list] (both [id→doc] in
    order), using an LCS so only genuinely-moved documents are repositioned. Each callback's
    [before] argument is the id to insert before, or [None] for the end of the list. *)
val diff_ordered :
  old_list:(string * Bson.t) list ->
  new_list:(string * Bson.t) list ->
  added_before:(string -> Bson.t -> string option -> unit) ->
  changed:(string -> Bson.t -> string list -> unit) ->
  moved_before:(string -> string option -> unit) ->
  removed:(string -> unit) ->
  unit
