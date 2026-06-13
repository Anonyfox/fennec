(** Routes a decoded DDP data delta onto the live {!Merge_store}. Shared by the DDP client(s); pure +
    native, so the wire→cache mapping is unit-testable without a browser. Control frames
    (ready / nosub / ping) are the client's concern and are not handled here.

    {[ (* the DDP client routes each decoded frame: data deltas hit the store, *)
       (* control frames fall through to the client's own handler *)
       if Wire_route.apply_delta box m then ()
       else handle_control_frame m ]} *)

(** [apply_delta box m] applies [m] to [box] when it is a data delta — [added] / [changed] /
    [removed] and the ordered [addedBefore] / [movedBefore] — and returns [true]. [addedBefore]
    surfaces the document (ordering is left to [find ~sort]); [movedBefore] is a no-op (a pure
    reorder carries no fields). Returns [false] for any non-data message so the caller can handle the
    control frames itself. *)
val apply_delta : Merge_store.t -> Fennec_ddp.Message.t -> bool
