(** The collection DECLARATION — pure and instance-free, shared by server and browser (what the
    [@@deriving collection] deriver generates): name + shape + indexes. The server attaches it to a
    reactive instance at boot; the browser binds it to the live client.

    {[ (* store/task.ml — the WHOLE model in one file *)
       type t = {
         id : string;
         title : string;   [@non_empty] [@max_len 200]
         status : string;  [@one_of [ "todo"; "doing"; "done" ]]
       }
       [@@deriving collection ~name:"tasks"]
       let () = [%index unique title]                         (* declared, reconciled at boot *)
       (* downstream: Task.find ~where:[%q status = "doing"] () *) ]}
*)

(** The declaration of a collection of ['a] documents. *)
type 'a t

(** [v ?indexes name codec] declares the collection [name] with document shape [codec] (and optional
    indexes). Every declaration is also recorded for {!declared}. *)
val v : ?indexes:Index.t list -> string -> 'a Sift.t -> 'a t

(** A declaration with its document type hidden — the element of {!declared}. *)
type any = Any : 'a t -> any

(** Every collection declared so far (each {!v} call, in declaration order) — what the server opens at
    boot, before any workflow transaction runs, so no transaction ever has to create a collection. *)
val declared : unit -> any list

(** The collection's name. *)
val name : 'a t -> string

(** The document shape that writes validate against and reads decode through. *)
val codec : 'a t -> 'a Sift.t

(** The indexes passed to {!v} (see {!all_indexes} for the full set). *)
val indexes : 'a t -> Index.t list

(** The mongod validator document derived from the shape ({!Json_schema.validator}). *)
val validator : 'a t -> Bson.t

(** [index collection [ … ]] declares the collection's indexes, co-located with the model:
    [Def.index collection Index.[ unique [ asc Fields.email ]; asc Fields.team ]]. Reconciled at
    boot by the runtime. *)
val index : 'a t -> Index.t list -> unit

(** Every declared index (those passed to {!v} plus those via {!index}). *)
val all_indexes : 'a t -> Index.t list
