(** The post-tool hook command ([fennec agent hook --harness <id>]): block for the next dev verdict
    ({!Journal.feedback_text}) and wrap it in the calling harness's injection JSON
    ({!Harness.render_feedback}) — the single per-harness output step. *)

val run : harness:Harness.t -> dir:string -> timeout:float -> input:string -> string
