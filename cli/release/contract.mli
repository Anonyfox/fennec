(** The deploy contract — what [fennec release] prints once the artifact is staged, plus an optional
    runtime Dockerfile. The staged binary is {e production by default} (Fennec keys dev/prod off the
    native-vs-bytecode build, not an env var; see {!Paw.Dev_proto.is_dev}), so the contract is just the
    genuinely deploy-specific environment it reads — above all a [MONGO_URL] and the listen port.
    Surfacing it is half the point of the command: the binary is the easy part. *)

(** The runtime environment the production binary reads, in the order shown to the user: the
    deploy-specific values first ([MONGO_URL], [FENNEC_PORT], [MAIL_URL]), then [FENNEC_ENV] {e last} —
    the optional override it now is (a native release is already production; set [development] only to
    run it in dev mode locally). *)
val env_table : (string * string) list

(** [human_size bytes] is a compact size like ["44.1 MB"] / ["9.5 KB"] / ["512 B"]. *)
val human_size : int -> string

(** [render ~path ~bytes ~embedded] is the staged-artifact summary + a ready-to-paste run line + the
    {!env_table}. [embedded] is [Some n] (a web app, [n] files baked in) or [None] (API/SSR-only). *)
val render : path:string -> bytes:int -> embedded:int option -> string

(** [dockerfile ~name ~bin] is a runtime Dockerfile that copies the staged binary [bin] (a path relative
    to the Docker build context / project root; a leading ["./"] is dropped) and runs it. Documents the
    cross-OS caveat — the binary must be built for the image's platform. *)
val dockerfile : name:string -> bin:string -> string
