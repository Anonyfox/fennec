(** The HTTP access logger — one structured event ({!Access.t}), rendered in the format the
    environment calls for. A thin rendering + wiring layer over {!Access} (which the server captures
    once per request, with the final status + post-gzip size + microsecond duration).

    {[ (* dev: pretty, colourised; prod: NDJSON; both auto-selected by env *)
       Endpoint.pipe [ Request_id.make (); Logger.make () ]

       (* force a format / a sink *)
       Logger.make ~format:Logfmt ()
       Logger.make ~sink:(output_string my_log_file) () ]}

    The logger PAW installs a per-request sink that the server invokes AFTER {!Responder.finalize}, so
    the line carries the real post-gzip byte count (a paw runs before finalize and could not see it).
    For a server you drive directly, pass {!sink} as [Server.run ~on_access]. *)

(** The output format. *)
type format =
  | Pretty   (** aligned, colourised columns for a human at a TTY (the dev default) *)
  | Json     (** NDJSON — one object per line, for a log shipper (the prod default) *)
  | Logfmt   (** [key=value] pairs; values with spaces are quoted *)
  | Apache   (** the combined access-log line; fields not carried render as ["-"] *)
  | Dev      (** the framed [\[fennec:http\]] wire line the dev supervisor parses (see {!Dev_proto}) *)

(** Build the logger paw. It installs a per-request access-log sink on the conn; the server calls it
    once the response is finalized.

    [format] overrides the env-derived default: [FENNEC_DEV_UI] set → {!Dev}; [FENNEC_ENV=production]
    → {!Json}; otherwise → {!Pretty}.

    [sink] overrides the per-format default destination and receives the plain line (never colour
    codes). The defaults: {!Pretty} → stderr (colourised when stderr is a TTY and [NO_COLOR] is
    unset); {!Json} / {!Logfmt} / {!Apache} / {!Dev} → stdout. Each default sink appends a newline. *)
val make : ?format:format -> ?sink:(string -> unit) -> unit -> Pipeline.t

(** The renderer as a plain [Access.t -> unit] — pass it as [Server.run ~on_access] for a server you
    drive yourself (same [?format] / [?sink] semantics as {!make}). *)
val sink : ?format:format -> ?sink:(string -> unit) -> unit -> Access.t -> unit

(** [render ?color fmt a] renders one event to a single line (no trailing newline). [color] (default
    [false]) only affects {!Pretty}. Exposed for testing / custom wiring. *)
val render : ?color:bool -> format -> Access.t -> string
