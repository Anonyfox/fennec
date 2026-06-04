(** HTTP Basic auth — answers 401 with a [WWW-Authenticate] challenge unless the request's
    credentials match. Credentials are compared in constant time. *)

(** Build the basic-auth paw guarding everything downstream. [realm] (default ["Restricted"])
    is shown in the browser's auth prompt. *)
val make : username:string -> password:string -> ?realm:string -> unit -> Fennec_paw.Paw.t
