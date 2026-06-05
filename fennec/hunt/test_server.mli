(* Spawn and manage a server process for testing. Tied to an Eio switch (structural cleanup).

   {[let server = Test_server.spawn ~sw ~net ~clock ~port:4000 ["fennec"; "dev"] in
     server |> get "/api/health" |> Expect.status 200 |> Expect.body_contains "ok"]} *)

(** A running server process. *)
type t

val pid : t -> int
val port : t -> int

(** Spawn a server process from [cmd] (an argv list), wait for it to accept TCP on [port],
    and return the handle. The process is killed when the Eio [sw] ends. [env] is appended to
    the inherited environment. [timeout] is the readiness deadline (default 30s). *)
val spawn :
  sw:Eio.Switch.t ->
  net:[ `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?timeout:float ->
  ?env:string array ->
  port:int ->
  string list ->
  t

(** Send a signal to the server process (returns the handle for chaining). *)
val signal : t -> int -> t

(** Block until nothing is listening on [port], or fail after [timeout] seconds. *)
val port_free_within : timeout:float -> int -> unit

(** Is something currently listening on [port]? (A non-blocking probe.) *)
val port_held : int -> bool

(** {1 Convenience request methods — pass the server's net + port automatically} *)

val get : t -> string -> Http_client.response
val get_h : t -> ?headers:(string * string) list -> string -> Http_client.response
val post : t -> ?body:string -> string -> Http_client.response
