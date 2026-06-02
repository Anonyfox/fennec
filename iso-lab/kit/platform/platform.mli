(* Platform: the native-vs-browser surface, resolved at LINK time via a dune virtual
   library — not runtime hook refs. Fur.native = inert stubs (SSR-safe by
   construction); Fur.browser = js_of_ocaml. You cannot link a client without a real
   platform, and browser code cannot reach the SSR build. *)

(* the event currently being dispatched (browser sets it around each handler) *)
val event_value : unit -> string
val event_checked : unit -> bool
val event_key : unit -> string
val event_prevent_default : unit -> unit

(* localStorage *)
val local_get : string -> string option
val local_set : string -> string -> unit
val local_remove : string -> unit

(* history *)
val push_state : string -> unit
