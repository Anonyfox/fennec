(** Give error responses a body when the handler left it empty. A handler can [Conn.set_status 403 c]
    (or 422, 429, …) without writing a body; this fills in ["<code> <reason>"] as text, or — with
    [render] — branded HTML/JSON pages. Only touches responses with status >= 400 and an empty body,
    so successful responses pass straight through. Cheap: the hook is built once.

    (Routing 404s and uncaught handler exceptions are rendered by {!Server}'s [~on_error] funnel; this
    paw covers error statuses a handler sets on its own conn.)

    {[ endpoint |> Endpoint.use (Status_pages.make ()) |> … ]} *)

(** Build the paw filling empty error bodies. [render status] returns a custom body for [status]
    ([None] falls back to the default ["<code> <reason>"] text); omit it for the default everywhere.
    [content_type] is set on the generated body when the response has none (default
    ["text/plain; charset=utf-8"]; pass ["text/html; charset=utf-8"] for HTML [render]ed pages). *)
val make : ?content_type:string -> ?render:(int -> string option) -> unit -> Pipeline.t
