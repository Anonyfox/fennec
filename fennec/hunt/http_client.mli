(* A minimal typed HTTP/1.1 test client — one request per connection, raw Eio sockets, zero
   external deps. Returns a structured response the assertion DSL pipes through.

   Not a general-purpose HTTP library. Exactly what a test needs: request → response → assert. *)

(** A fully-read HTTP response. *)
type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

(** Find the first header with the given name (case-insensitive). *)
val header_value : string -> response -> string option

(** {1 Request methods} — each opens a fresh TCP connection to [127.0.0.1:port], sends the
    request, reads the full response (Connection: close), and returns it. *)

val get : net:[ `Generic ] Eio.Net.ty Eio.Resource.t -> port:int -> ?headers:(string * string) list -> string -> response
val post : net:[ `Generic ] Eio.Net.ty Eio.Resource.t -> port:int -> ?headers:(string * string) list -> ?body:string -> string -> response
val put : net:[ `Generic ] Eio.Net.ty Eio.Resource.t -> port:int -> ?headers:(string * string) list -> ?body:string -> string -> response
val delete : net:[ `Generic ] Eio.Net.ty Eio.Resource.t -> port:int -> ?headers:(string * string) list -> string -> response
val head : net:[ `Generic ] Eio.Net.ty Eio.Resource.t -> port:int -> ?headers:(string * string) list -> string -> response
