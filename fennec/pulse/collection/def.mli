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

type 'a t

val v : ?indexes:Index.t list -> string -> 'a Codec.t -> 'a t
val name : 'a t -> string
val codec : 'a t -> 'a Codec.t
val indexes : 'a t -> Index.t list

(** The mongod validator document derived from the shape ({!Schema.validator}). *)
val validator : 'a t -> Bson.t

(** [index collection [ … ]] declares the collection's indexes, co-located with the model:
    [Def.index collection Index.[ unique [ asc Fields.email ]; asc Fields.team ]]. Reconciled at
    boot by the runtime. *)
val index : 'a t -> Index.t list -> unit

(** Every declared index (those passed to {!v} plus those via {!index}). *)
val all_indexes : 'a t -> Index.t list
