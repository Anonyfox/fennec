(** A sliding-window restart limiter, so a server that dies on boot (a runtime error, a busy
    port) can't hot-loop. Past [max] crashes within [window] seconds it gives up until the next
    good build clears the streak. Time is passed in, so the policy is pure and directly tested. *)

(** Active limiter state: the timestamps of recent crashes within the window. *)
type t

(** [create ?window ?max ()] — at most [max] (default 5) restarts within [window] seconds
    (default 10) before {!record} returns {!Give_up}. *)
val create : ?window:float -> ?max:int -> unit -> t

(** What the limiter recommends after recording a crash. *)
type decision =
  | Retry of float  (** back off this many seconds, then restart *)
  | Give_up  (** too many crashes too fast; wait for the next good build *)

(** Record a crash at [now] and decide what to do. [flat] (e.g. a port still being released)
    uses a fixed 1s backoff instead of the exponential one. *)
val record : t -> now:float -> ?flat:bool -> unit -> decision

(** A successful build is the user's fix — clear the streak. *)
val reset : t -> unit
