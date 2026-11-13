(** The post-tool hook command ([fennec agent hook --harness <id>]): block for the next dev verdict
    ({!Journal.feedback_text}) and wrap it in the calling harness's injection JSON
    ({!Harness.render_feedback}) — the single per-harness output step. *)

(** Compute the dev feedback for the edit described by [input] (the hook payload the harness wrote
    to stdin) in project [dir], waiting up to [timeout] seconds for the verdict, and render it in
    [harness]'s injection shape (empty output when there is nothing to report). *)
val run : harness:Harness.t -> dir:string -> timeout:float -> input:string -> string
