(** Response finalization applied to every response (dynamic and static):
    content-encoding negotiation (gzip/deflate, [Vary], compressible + min-size
    only), strong ETag + conditional 304, [Date], correct [Content-Length], and
    HEAD → empty body.

    {[
      let response = Responder.finalize ~now:(Unix.time ()) ~req response
    ]} *)

(** Re-exports {!Fennec_core.Http} for use in the responder's type signatures. *)
module H = Fennec_core.Http

(** Finalize [resp] for [req]. [now] is epoch seconds (for [Date] / conditional). *)
val finalize : ?now:float -> req:H.request -> H.response -> H.response
