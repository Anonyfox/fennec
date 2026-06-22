(** The deploy contract — what [fennec release] prints once the artifact is staged, plus an optional
    runtime Dockerfile. The build produces a self-contained binary, but a correct {e run} still depends
    on a handful of environment variables (above all [FENNEC_ENV=production], the switch that makes the
    server use its embedded assets instead of looking for them on disk). Surfacing that contract is half
    the point of the command: the binary is the easy part. *)

(** The runtime environment the production binary reads: each [(name, description)] in the order shown
    to the user. [FENNEC_ENV] leads because forgetting it is the classic silent prod misconfiguration. *)
val env_table : (string * string) list

(** [human_size bytes] is a compact size like ["44.1 MB"] / ["9.5 KB"] / ["512 B"]. *)
val human_size : int -> string

(** [render ~name ~path ~bytes ~embedded] is the staged-artifact summary + a ready-to-paste run line +
    the {!env_table}. [embedded] is [Some n] (a web app, [n] files baked in) or [None] (API/SSR-only). *)
val render : name:string -> path:string -> bytes:int -> embedded:int option -> string

(** [dockerfile ~name ~bin] is a runtime Dockerfile that copies the staged binary [bin] (a path
    relative to the Docker build context / project root) and runs it in production. Documents the
    cross-OS caveat — the binary must be built for the image's platform. *)
val dockerfile : name:string -> bin:string -> string
