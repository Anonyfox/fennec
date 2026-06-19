(* The always-on [__currentUser] publication — the central proof that a SERVER-SIDE user mutation
   reaches a [__currentUser] subscriber over the live data path (accounts write → shared
   accounts_users handle → backend observe → reactive beat). A standalone Eio exe over MONGO_URL=
   :memory:, so it also proves STEP 1 (the shared in-memory handle) end to end: the Accounts store
   and the publication's reactive cursor open "accounts_users" by the same name and MUST see one
   store, or none of these beats arrive.

   It drives the PUBLIC Accounts API (the same calls userland makes) for every write, and reads the
   publication exactly as the DDP session does — through [Fennec_pulse_app.R.run_publication]. It
   asserts: (a) a subscriber for user U gets U's safe doc; (b) set_profile / set_roles / add_email
   each push the changed fields to the subscriber; (c) the delivered doc NEVER carries [services] or
   [passwordHash], even though the stored password user has them. *)

module A = Fennec_accounts.Accounts
module Pulse = Fennec_pulse_app
module R = Pulse.R
module Rx = Fennec_pulse.Reactive (* the module-level [beat] variant lives here, not in REACTIVE *)
module B = Bson

let failf fmt = Printf.ksprintf (fun s -> prerr_endline ("currentUser FAIL: " ^ s); exit 1) fmt
let ok label = function Ok v -> v | Error _ -> failf "%s returned Error" label

(* a tiny in-memory mirror of what a DDP session's sink would build from the beats: id -> fields.
   Added installs; Changed merges (and clears); Removed drops. We read it after each mutation. *)
type box = (string, (string * B.t) list) Hashtbl.t

let apply (box : box) : Rx.beat -> unit = function
  | Rx.Added { id; fields; _ } -> Hashtbl.replace box id fields
  | Rx.Changed { id; fields; cleared; _ } ->
    let cur = match Hashtbl.find_opt box id with Some f -> f | None -> [] in
    let cur = List.filter (fun (k, _) -> not (List.mem k cleared)) cur in
    let merged = List.fold_left (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc) cur fields in
    Hashtbl.replace box id merged
  | Rx.Removed { id; _ } -> Hashtbl.remove box id

let doc_of box id = match Hashtbl.find_opt box id with Some f -> f | None -> failf "no doc for %s in the sub" id
let field box id k = List.assoc_opt k (doc_of box id)
let has_field box id k = List.mem_assoc k (doc_of box id)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Mirage_crypto_rng_unix.use_default ();
  Unix.putenv "MONGO_URL" ":memory:";
  Unix.putenv "FENNEC_ACCOUNTS_SECRET" "current-user-pub-secret-0123456789";
  Fennec_mongo_dynamic.Dynamic.set_switch sw;
  A.start ~config:A.defaults ();
  A.boot ();
  let a = A.current () in

  (* the publication is registered transparently at Fennec_pulse_app module init — assert it is there
     (any app linking the facade gets it; no userland call) *)
  if not (List.mem "__currentUser" (R.publications ())) then
    failf "__currentUser is not registered (expected it always-on at app init)";

  (* a password user — so the STORED doc carries passwordHash, the secret the projection must drop *)
  let user = ok "create_user" (A.create_user a ~username:"Ada" ~email:"ada@example.com" ~password:"pw" ()) in
  let uid = user.A.id in

  (* (a) subscribe to __currentUser for U → the initial state is delivered synchronously by
     run_publication (the cursor replays its one matching doc as Added before returning) *)
  let box : box = Hashtbl.create 4 in
  let h = R.run_publication "__currentUser" ~user_id:(Some uid) ~params:[] ~on:(apply box) in
  (match field box uid "username" with
  | Some (B.String "Ada") -> ()
  | _ -> failf "subscriber did not receive the user's username");
  if field box uid "emails" = None then failf "subscriber did not receive the user's emails";

  (* (c) NO secrets ever — even though this is a password user with a stored passwordHash *)
  if has_field box uid "passwordHash" then failf "passwordHash LEAKED to the subscriber";
  if has_field box uid "services" then failf "services LEAKED to the subscriber";
  if has_field box uid "usernameLower" then failf "usernameLower (shadow) leaked to the subscriber";
  if has_field box uid "emailsLower" then failf "emailsLower (shadow) leaked to the subscriber";

  (* (b) THE POINT — mutate the user SERVER-SIDE and watch the change reach the subscriber live.
     set_profile *)
  let profile = B.doc [ ("displayName", B.str "Ada Lovelace"); ("theme", B.str "dark") ] in
  let _ = ok "set_profile" (A.set_profile a uid (Some profile)) in
  (match field box uid "profile" with
  | Some (B.Document _ as p) when B.get_string p "displayName" = Some "Ada Lovelace" -> ()
  | _ -> failf "set_profile did not reach the subscriber");

  (* set_roles *)
  let _ = ok "set_roles" (A.set_roles_from_strings a uid [ "admin"; "editor" ]) in
  (match field box uid "roles" with
  | Some (B.Array roles) ->
    let names = List.filter_map (function B.String s -> Some s | _ -> None) roles in
    if not (List.mem "admin" names && List.mem "editor" names) then
      failf "set_roles change reached the subscriber but without the expected roles"
  | _ -> failf "set_roles did not reach the subscriber");

  (* add_email *)
  let _ = ok "add_email" (A.add_email a uid "ada2@example.com") in
  (match field box uid "emails" with
  | Some (B.Array emails) ->
    let addrs = List.filter_map (fun d -> B.get_string d "address") emails in
    if not (List.mem "ada2@example.com" addrs) then failf "add_email change did not carry the new address"
  | _ -> failf "add_email did not reach the subscriber");

  (* secrets STILL absent after all the mutations (a Changed must never have re-introduced them) *)
  if has_field box uid "passwordHash" then failf "passwordHash leaked via a Changed beat";
  if has_field box uid "services" then failf "services leaked via a Changed beat";

  (h : Rx.live_handle).stop ();

  (* a second subscriber, scoped to None (anonymous), must see NOTHING — the never-match selector *)
  let abox : box = Hashtbl.create 1 in
  let ah = R.run_publication "__currentUser" ~user_id:None ~params:[] ~on:(apply abox) in
  if Hashtbl.length abox <> 0 then failf "anonymous (user_id=None) subscriber received documents";
  (ah : Rx.live_handle).stop ();

  (* ---- STEP 3 proof: an in-session identity change re-scopes __currentUser ---------------------
     Drive the REAL DDP session (Fennec_pulse_server over the app's R, the SAME stack the websocket
     paw runs) through a fake channel. A login method's set_user_id rebinds the connection and the
     server pushes Msg.User; the client reacts by RE-SENDING Sub for every live sub (resubscribe_all),
     which makes the server stop the old pub and re-run it under the new user_id. We model exactly that
     re-Sub here and assert: anon sub is empty → after login + re-Sub the user's doc appears → after
     logout + re-Sub it is removed. *)
  let module Srv = Fennec_pulse_server.Make (R) in
  let module Msg = Fennec_ddp.Message in
  let module Ws = Paw.Ws_channel in
  (* test login/logout verbs: the only thing that matters here is that they flip the connection user
     via set_user_id, exactly as the native [login]/[logout] do *)
  R.methods
    [ ("test_login", fun (inv : R.invocation) args ->
         (match args with [ B.String u ] -> inv.set_user_id (Some u) | _ -> ());
         B.Null);
      ("test_logout", fun (inv : R.invocation) _ -> inv.set_user_id None; B.Null) ];
  let out = ref [] in
  let ch = { Ws.send = (fun s -> out := s :: !out); on_text = (fun _ -> ()); on_close = (fun () -> ()) } in
  let emitted () = List.rev_map Msg.decode !out in
  Srv.serve ~session_id:"S-rescope" ch;
  ch.Ws.on_text (Msg.encode (Msg.Connect { session = None; version = "1"; support = [ "1" ] }));
  (* anonymous sub → no user doc for uid *)
  ch.Ws.on_text (Msg.encode (Msg.Sub { id = "cu"; name = "__currentUser"; params = []; have = None }));
  let has_user_added ms =
    List.exists
      (function Msg.Added { sub = Some "cu"; id; _ } -> id = uid | _ -> false) ms
  in
  let has_user_removed ms =
    List.exists (function Msg.Removed { sub = Some "cu"; id; _ } -> id = uid | _ -> false) ms
  in
  if has_user_added (emitted ()) then failf "anonymous __currentUser sub leaked the user doc over the wire";

  (* LOGIN: the method rebinds the connection, the server emits Msg.User { id = uid } *)
  out := [];
  ch.Ws.on_text
    (Msg.encode (Msg.Method { method_ = "test_login"; params = [ B.str uid ]; id = "ml"; random_seed = None }));
  if not (List.exists (function Msg.User { id = Some u } -> u = uid | _ -> false) (emitted ())) then
    failf "login did not push Msg.User to the client";

  (* the client's reaction: re-Sub every live sub (this is resubscribe_all). Now the server re-runs
     __currentUser under the NEW user_id → the user's doc is added. *)
  out := [];
  ch.Ws.on_text (Msg.encode (Msg.Sub { id = "cu"; name = "__currentUser"; params = []; have = None }));
  if not (has_user_added (emitted ())) then
    failf "after login + re-Sub, __currentUser did not deliver the user's doc";

  (* LOGOUT: set_user_id None → Msg.User None; the re-Sub (delta-resync, holding the user doc) makes
     the server's resync ready emit removed for the now-unmatched doc *)
  out := [];
  ch.Ws.on_text
    (Msg.encode (Msg.Method { method_ = "test_logout"; params = []; id = "mo"; random_seed = None }));
  if not (List.exists (function Msg.User { id = None } -> true | _ -> false) (emitted ())) then
    failf "logout did not push Msg.User None to the client";
  out := [];
  (* the client holds the user doc; a delta-resync Sub declares it, and the now-empty pub removes it *)
  let have = [ ("accounts_users", [ (uid, "anyhash") ]) ] in
  ch.Ws.on_text (Msg.encode (Msg.Sub { id = "cu"; name = "__currentUser"; params = []; have = Some have }));
  if not (has_user_removed (emitted ())) then
    failf "after logout + re-Sub, __currentUser did not remove the user's doc";

  Unix.putenv "MONGO_URL" "";
  print_endline
    "__currentUser publication: OK (initial safe doc + live set_profile/set_roles/add_email deltas + \
     no passwordHash/services leak + empty for anonymous + login/logout re-scope over a live DDP \
     session)"
