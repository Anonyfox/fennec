(* Deterministic dev port allocation from a single base. The gateway (prod-identical Host router)
   is at the base; endpoint i (declaration order) is at base+1+i. Validated up front so the
   accessors are total. See port_plan.ml. *)

type t

(** [of_base ~base ~count] for [count] endpoints. [Error msg] if [base] is out of range (1..65535)
    or the block [base .. base+count] would run past 65535. *)
val of_base : base:int -> count:int -> (t, string) result

(** The gateway port — where the prod-identical Host router listens in dev (= [base]). *)
val gateway : t -> int

(** The forced convenience port for endpoint [index] (0-based, declaration order) = [base+1+index]. *)
val endpoint_port : t -> index:int -> int

(** The endpoint count this plan was built for. *)
val count : t -> int
