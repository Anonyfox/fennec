(** The console wire protocol — the seam between a console {e frontend} (the terminal driver, or a
    future web / editor surface) and the in-server eval {e engine}.

    Transport-agnostic: every message is a self-delimiting, length-prefixed {e frame} of bytes, so a
    frontend can speak it over a Unix socket (the terminal driver's [select] loop) or an Eio flow (the
    engine) without this module knowing which. The payload is {!Stdlib.Marshal}ed — both ends link
    this same library, so the message {e types are} the contract and no hand-written serializer can
    drift from them. (Same-build only: the frontend and engine are always the one [fennec] version.) *)

(** {1 Messages} *)

(** A request from a frontend to the engine. [id] correlates an {!Eval}/{!Complete} with its replies. *)
type request =
  | Hello of { cols : int }
      (** opening handshake; [cols] is the terminal width, for pretty-printing. The engine replies
          with exactly one {!Banner}. *)
  | Eval of { id : int; phrase : string }
      (** evaluate one toplevel [phrase] (a trailing [;;] is optional — the engine adds it). *)
  | Cancel of { id : int }
      (** cancel the in-flight {!Eval} carrying this [id] (Ctrl-C on a hung evaluation). *)
  | Complete of { id : int; prefix : string }
      (** request completions for [prefix] (the identifier under the cursor, e.g. ["Pulse.fi"]). *)

(** The welcome context, sent once in reply to {!Hello}. *)
type banner = {
  app : string;  (** the app / project name *)
  backend : string;  (** the resolved data backend (the burrow path, a mongo URL, or [:memory:]) *)
  version : string;  (** the framework version *)
  opened : string list;  (** the modules opened by the REPL preamble (e.g. [["Fennec"; "Paw"]]) *)
}

(** A reply from the engine to a frontend. One {!Eval} produces zero or more {!Output} followed by a
    single {!Done}; one {!Complete} produces one {!Completions}. *)
type reply =
  | Banner of banner
  | Output of { id : int; text : string; is_error : bool }
      (** a chunk of an eval's output: the toplevel result line ([- : t = v]), the phrase's own
          printed side effects, or — when [is_error] — a type error / raised exception. May arrive
          in several chunks; concatenate them in order. *)
  | Done of { id : int }  (** the {!Eval} with this [id] has finished (whether it succeeded or not). *)
  | Completions of { id : int; items : string list }

(** {1 Framing} *)

(** A single self-delimiting frame (a 4-byte big-endian length prefix + the marshaled payload), ready
    to write to the transport in one go. *)
val frame_request : request -> string

val frame_reply : reply -> string

(** Incremental frame reader: feed it whatever bytes arrive — partial frames are fine — and pull whole
    messages out as they complete. Use one reader per connection and direction. *)
module Reader : sig
  (** A reader decoding frames into ['a] (instantiated at {!request} or {!reply}). *)
  type 'a t

  (** A reader for the engine side (it reads {!request}s). *)
  val for_requests : unit -> request t

  (** A reader for the frontend side (it reads {!reply}s). *)
  val for_replies : unit -> reply t

  (** [feed t bytes] appends freshly received bytes to the reader's buffer. *)
  val feed : 'a t -> string -> unit

  (** [next t] returns the next complete message, or [None] when more bytes are needed. Call it in a
      loop after each {!feed} until it returns [None].
      @raise Failure on a corrupt or implausibly large frame (a safety cap, not a real limit). *)
  val next : 'a t -> 'a option
end
