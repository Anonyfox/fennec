(* The wire→cache router: maps a decoded DDP data delta onto the live merge store. Shared by the DDP
   client(s); control messages (ready / nosub / ping) stay in the client since they touch its
   subscription state. Pure + native, so the wire→cache mapping is unit-testable without a browser.

   [Added_before] / [Moved_before] are the DDP spec's ordered-collection deltas. Neither side of a
   fennec↔Meteor pairing actually emits them — the spec itself notes the ordered messages "are not
   currently used by Meteor", and a fennec server likewise sends only added/changed/removed (clients
   re-sort via [find ~sort], so windowed/sorted publications need no wire ordering). They are handled
   here for spec-general DDP servers: the DOCUMENT is surfaced (membership preserved); the positional
   hint is dropped, exactly as a Mongo-backed Meteor client effectively treats it — a caveat only for
   a non-Meteor server whose collection order is not derivable from document fields. *)

module Msg = Fennec_ddp.Message

(* [apply_delta box m] applies a data-delta message to [box] and returns [true] when [m] was a data
   delta (so a caller can fall through to control messages on [false]). The "" sub tag is the
   standard-DDP / ordered-delta default — Meteor ignores it and per-field precedence collapses to one. *)
let apply_delta (box : Merge_store.t) (m : Msg.t) : bool =
  (* Standard-DDP deltas carry no sub tag (a real Meteor server, or our ordered addedBefore): they
     collapse to the "" sub. That is CORRECT, not a collision — in standard mode the SERVER runs the
     per-connection merge box and sends ONE already-merged stream per collection, so "" is that single
     authoritative view (there is no client-side refcount to defeat). fennec's own extended mode always
     tags every delta with a real sub id, so tagged and untagged never mix on one connection. *)
  let s = function Some s -> s | None -> "" in
  match m with
  | Msg.Added { collection; id; fields; sub } ->
      Merge_store.added box ~sub:(s sub) ~collection ~id ~fields;
      true
  | Msg.Changed { collection; id; fields; cleared; sub } ->
      Merge_store.changed box ~sub:(s sub) ~collection ~id ~fields ~cleared;
      true
  | Msg.Removed { collection; id; sub } ->
      Merge_store.removed box ~sub:(s sub) ~collection ~id;
      true
  | Msg.Added_before { collection; id; fields; _ } ->
      (* ordered add — surface the doc; ordering is a [find ~sort] concern, not a positional cache *)
      Merge_store.added box ~sub:"" ~collection ~id ~fields;
      true
  | Msg.Moved_before _ -> true (* pure reorder, no field data — nothing to cache *)
  | _ -> false
