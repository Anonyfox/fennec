(* The dev-outbox model — the captured shape of a sent email. On the SERVER (dev only) [Mail.set_dev_capture]
   mirrors every send into the "_fennec_outbox" collection; in the BROWSER [Outbox_view] subscribes and
   renders it live. Same [@@deriving collection] machinery as the app's own models (see [Task]) — one record,
   shared verbatim by the server binary and the JS bundle. The "_" name marks it framework-internal. *)

type t = {
  id : string;
  sender : string;      (* the From header, rendered *)
  recipients : string;  (* To (+ Cc), comma-joined *)
  subject : string;
  text : string;        (* the plain-text body, or "" *)
  html : string;        (* the HTML body, or "" *)
  received : string;    (* server capture time "YYYY-MM-DD HH:MM:SS" UTC — fixed-width, so it sorts by recency *)
}
[@@deriving collection ~name:"_fennec_outbox"]
