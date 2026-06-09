(* The wire→cache router: maps a decoded DDP data delta onto the live merge store. Shared by the DDP
   client(s); control messages (ready / nosub / ping) stay in the client since they touch its
   subscription state. Pure + native, so the wire→cache mapping is unit-testable without a browser.

   [Added_before] / [Moved_before] are Meteor's ordered-observe deltas. A fennec server emits only
   added/changed/removed, but a real Meteor server (V1 drop-in) can send ordered ones — so we SURFACE
   the document (ordering is honored by [find ~sort]) instead of dropping it on the floor. *)

module Msg = Fennec_ddp.Message

(* [apply_delta box m] applies a data-delta message to [box] and returns [true] when [m] was a data
   delta (so a caller can fall through to control messages on [false]). The "" sub tag is the
   standard-DDP / ordered-delta default — Meteor ignores it and per-field precedence collapses to one. *)
let apply_delta (box : Merge_store.t) (m : Msg.t) : bool =
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
