(** Native-vs-browser surface, resolved at {b link time} via a dune virtual library — not via
    runtime hook refs. [Fur.native] provides inert SSR-safe stubs; [Fur.browser] provides the
    js_of_ocaml implementations. A client bundle cannot be linked without a real platform, and
    browser code cannot reach the SSR build.

    Most code reaches this surface indirectly (through {!Fur.Data} and DOM event helpers).
    The one direct use is per-request isolation in the SSR driver: wrap each render in
    {!with_data_context} so concurrent requests never share a seed table, then read the
    accumulated seed back via {!seed_table}:

    {[
      let html =
        with_data_context (fun () ->
          set_data_source fetch_resource;   (* key -> callback -> unit *)
          let body = render_app () in       (* resources fill the seed during render *)
          shell ~seed:(seed_table ()) body)
    ]} *)

(** {1 Ambient event (read inside DOM event handlers)} *)

(** The [event.target.value] of the currently dispatched event. Call inside a DOM event handler. *)
val event_value : unit -> string

(** The [event.target.checked] state. Call inside a checkbox event handler. *)
val event_checked : unit -> bool

(** The [event.key] string. Call inside a keyboard event handler. *)
val event_key : unit -> string

(** Call [event.preventDefault()] on the currently dispatched event. *)
val event_prevent_default : unit -> unit

(** {1 localStorage} *)

(** Read a value from [localStorage]; [None] when absent or in SSR. *)
val local_get : string -> string option

(** Write a value to [localStorage]. No-op in SSR. *)
val local_set : string -> string -> unit

(** Remove a key from [localStorage]. No-op in SSR. *)
val local_remove : string -> unit

(** {1 History API} *)

(** Push a new URL onto the browser's history stack ([window.history.pushState]).
    No-op in SSR (path changes are handled by the router's signal directly). *)
val push_state : string -> unit

(** {1 Per-request SSR data context} *)

(** [with_data_context f] runs [f] with a fresh, isolated per-request data context (the seed table +
    the fetch source). On the concurrent native server it is {b fiber-local}, so simultaneous SSR
    requests never share a seed table; on the browser (one document) it is the single global context.
    Outside an Eio run the accessors below degrade to a process-global context (single-threaded, safe). *)
val with_data_context : (unit -> 'a) -> 'a

(** The current request's seed table ([key → JSON-string payload]). *)
val seed_table : unit -> (string, string) Hashtbl.t

(** The current request's fetch source (resolves a resource key to a JSON payload via a callback). *)
val data_source : unit -> string -> (string -> unit) -> unit

(** Replace the current request's fetch source. *)
val set_data_source : (string -> (string -> unit) -> unit) -> unit
