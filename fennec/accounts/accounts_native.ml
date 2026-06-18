(* accounts_native.ml — the storage backends + the process-native singleton, all moved VERBATIM
   from accounts.ml.

   It assembles, on top of the engine ({!Accounts_base}) and the backend-blind collection store
   ({!Accounts_collection_store}):
     - the in-memory backend: [memory_user_store] / [memory_token_store] / [memory_store];
     - [module Store]: the unified store over ANY runtime-selected backend (minimongo / embedded
       Burrow / mongod, all behind {!Backend_dyn}), [Store.unavailable], [Store.minimongo], the index
       DDL, and the per-facet accessors;
     - the native shell: [native_secret] / [native_store] / [make_native] / [current] (the memoized
       process singleton) / [native_paw] / [boot].
   The facade re-exports all of these so the public surface ([Accounts.Store], [Accounts.boot],
   [Accounts.native_paw], [Accounts.current], [Accounts.memory_store], …) is unchanged.

   The shared in-memory TEST FIXTURE lives here, at the lowest module that can build it: it needs
   [make] (engine) AND an assembled in-memory [store] (this module's [memory_store] / [Store.minimongo])
   — both of which sit ABOVE the whole engine, so the fixture cannot be pushed down into the engine
   leaves. The store/backend [let%test] blocks ride along here; the engine-integration and DDP-method
   suites that also build on this fixture live in the modules that open this one. *)

open Accounts_base
module Collection_store = Accounts_collection_store

let memory_user_store () =
  let users : (user_id, user) Hashtbl.t = Hashtbl.create 64 in
  let passwords : (user_id, string) Hashtbl.t = Hashtbl.create 64 in
  let m = Mutex.create () in
  let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f in
  let find_unlocked (pred : user -> bool) = Hashtbl.to_seq_values users |> Seq.find pred in
  let find pred = locked (fun () -> find_unlocked pred) in
  let user_has_email email (u : user) =
    List.exists (fun e -> normalize_email e.address = email) u.emails
  in
  let user_has_username username (u : user) =
    option_exists (fun n -> normalize_username n = username) u.username
  in
  let find_user_by_id id = locked (fun () -> Ok (Hashtbl.find_opt users id)) in
  let find_user_by_email email =
    let email = normalize_email email in
    Ok (find (user_has_email email))
  in
  let find_user_by_username username =
    let username = normalize_username username in
    Ok (find (user_has_username username))
  in
  let exists_email_unlocked email =
    match find_unlocked (user_has_email (normalize_email email)) with Some _ -> true | None -> false
  in
  let exists_username_unlocked username =
    match find_unlocked (user_has_username (normalize_username username)) with Some _ -> true | None -> false
  in
  let exists_other_email_unlocked id email =
    match find_unlocked (fun u -> u.id <> id && user_has_email (normalize_email email) u) with Some _ -> true | None -> false
  in
  let exists_other_username_unlocked id username =
    match find_unlocked (fun u -> u.id <> id && user_has_username (normalize_username username) u) with
    | Some _ -> true
    | None -> false
  in
  let find_user_by_service ~strategy ~service_id =
    Ok
      (find (fun u ->
           match List.assoc_opt strategy u.services with
           | Some svc -> doc_get_string svc "id" = Some service_id
           | None -> false))
  in
  let create_user u ~password_hash =
    locked (fun () ->
        match validate_user_shape u with
        | Error _ as e -> e
        | Ok () ->
          if Hashtbl.mem users u.id then Error (Store_error ("duplicate user id: " ^ u.id))
          else
          match
            List.find_map
              (fun e ->
                let email = normalize_email e.address in
                if exists_email_unlocked email then Some (Duplicate_email email) else None)
              u.emails
          with
          | Some e -> Error e
          | None -> (
            match u.username with
            | Some username when exists_username_unlocked username ->
              Error (Duplicate_username (normalize_username username))
            | _ ->
              Hashtbl.add users u.id u;
              Option.iter (Hashtbl.replace passwords u.id) password_hash;
              Ok u))
  in
  let update_user u =
    locked (fun () ->
        match validate_user_shape u with
        | Error _ as e -> e
        | Ok () ->
          if not (Hashtbl.mem users u.id) then Error User_not_found
          else
          match
            List.find_map
              (fun e ->
                let email = normalize_email e.address in
                if exists_other_email_unlocked u.id email then Some (Duplicate_email email) else None)
              u.emails
          with
          | Some e -> Error e
          | None -> (
            match u.username with
            | Some username when exists_other_username_unlocked u.id username ->
              Error (Duplicate_username (normalize_username username))
            | _ ->
              let updated = { u with updated_at = now () } in
              Hashtbl.replace users u.id updated;
              Ok updated))
  in
  let password_hash id = locked (fun () -> Ok (Hashtbl.find_opt passwords id)) in
  let set_password_hash id hash =
    locked (fun () ->
        if Hashtbl.mem users id then (Hashtbl.replace passwords id hash; Ok ()) else Error User_not_found)
  in
  let set_password_hash_and_bump id hash =
    locked (fun () ->
        match Hashtbl.find_opt users id with
        | None -> Error User_not_found
        | Some u ->
          let epoch = u.auth_epoch + 1 in
          Hashtbl.replace passwords id hash;
          Hashtbl.replace users id { u with auth_epoch = epoch; updated_at = now () };
          Ok epoch)
  in
  let bump_auth_epoch id =
    locked (fun () ->
        match Hashtbl.find_opt users id with
        | None -> Error User_not_found
        | Some u ->
          let epoch = u.auth_epoch + 1 in
          Hashtbl.replace users id { u with auth_epoch = epoch; updated_at = now () };
          Ok epoch)
  in
  { find_user_by_id; find_user_by_email; find_user_by_username; find_user_by_service; create_user; update_user; password_hash; set_password_hash; set_password_hash_and_bump; bump_auth_epoch }

(* In-process login-token store for the memory backend: a [sid]-keyed table of
   ([session_info] * hashed-token). Mirrors {!memory_user_store}; cannot fail. *)
let memory_token_store () : token_store =
  let rows : (string, session_info * string) Hashtbl.t = Hashtbl.create 64 in
  let m = Mutex.create () in
  let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f in
  let record (s : session_info) ~hashed = locked (fun () -> Hashtbl.replace rows s.session_id (s, hashed); Ok ()) in
  let find_live ~sid ~hashed ~now =
    locked (fun () ->
        match Hashtbl.find_opt rows sid with
        | Some ((info : session_info), h) when info.expires_at > now && h = hashed -> Ok (Some info)
        | _ -> Ok None)
  in
  let list_for_user user_id ~now =
    locked (fun () ->
        Ok
          (Hashtbl.fold
             (fun _ ((info : session_info), _) acc ->
               if info.user_id = user_id && info.expires_at > now then info :: acc else acc)
             rows []
          |> List.sort (fun (a : session_info) (b : session_info) -> compare b.created_at a.created_at)))
  in
  let touch ~sid ~now =
    locked (fun () ->
        (match Hashtbl.find_opt rows sid with
        | Some (info, h) -> Hashtbl.replace rows sid ({ info with last_active_at = now }, h)
        | None -> ());
        Ok ())
  in
  let revoke ~sid =
    locked (fun () ->
        let existed = Hashtbl.mem rows sid in
        Hashtbl.remove rows sid;
        Ok existed)
  in
  let revoke_user user_id ?keep () =
    locked (fun () ->
        let to_remove =
          Hashtbl.fold
            (fun sid ((info : session_info), _) acc ->
              if info.user_id = user_id && Some sid <> keep then sid :: acc else acc)
            rows []
        in
        List.iter (Hashtbl.remove rows) to_remove;
        Ok (List.length to_remove))
  in
  let gc_expired ~now =
    locked (fun () ->
        let to_remove =
          Hashtbl.fold (fun sid ((info : session_info), _) acc -> if info.expires_at <= now then sid :: acc else acc) rows []
        in
        List.iter (Hashtbl.remove rows) to_remove;
        Ok (List.length to_remove))
  in
  { record; find_live; list_for_user; touch; revoke; revoke_user; gc_expired }

let memory_store () =
  {
    users = memory_user_store ();
    tokens = memory_token_store ();
    identities = Identity.memory_store ();
    challenges = Challenge.memory_store ();
    passkeys = Passkey.memory_store ();
    orgs = Org.memory_store ();
    mfa = Mfa.memory_store ();
    scim = Scim.memory_store ();
    audit = Audit.memory_store ();
    ensure_indexes = (fun () -> ());
  }

module Store = struct
  type t = store
  type user = user_store

  let unavailable ?(message = Mongo_runtime.unavailable_message ()) () =
    let store_error = Store_error message in
    let error = Error store_error in
    let string_error = Error message in
    let challenge_error = Error (Challenge.Store_error message) in
    let users =
      {
        find_user_by_id = (fun _ -> error);
        find_user_by_email = (fun _ -> error);
        find_user_by_username = (fun _ -> error);
        find_user_by_service = (fun ~strategy:_ ~service_id:_ -> error);
        create_user = (fun _ ~password_hash:_ -> error);
        update_user = (fun _ -> error);
        password_hash = (fun _ -> error);
        set_password_hash = (fun _ _ -> error);
        set_password_hash_and_bump = (fun _ _ -> error);
        bump_auth_epoch = (fun _ -> error);
      }
    in
    let tokens =
      {
        record = (fun _ ~hashed:_ -> error);
        find_live = (fun ~sid:_ ~hashed:_ ~now:_ -> error);
        list_for_user = (fun _ ~now:_ -> error);
        touch = (fun ~sid:_ ~now:_ -> error);
        revoke = (fun ~sid:_ -> error);
        revoke_user = (fun _ ?keep:_ () -> error);
        gc_expired = (fun ~now:_ -> error);
      }
    in
    let identities =
      let conflict ?verified_at ~created_at ~user_id key =
        Identity.Conflict (Identity.link ?verified_at ~user_id key ~created_at)
      in
      {
        Identity.find = (fun _ -> None);
        list = (fun ?user_id:_ () -> []);
        attach = conflict;
        detach = (fun ?allow_last:_ ~user_id:_ _ -> Identity.Link_not_found);
        merge = (fun ~from_user_id:_ ~into_user_id:_ -> Ok { Identity.from_user_id = ""; into_user_id = ""; move = []; keep = []; conflicts = [] });
      }
    in
    let challenges =
      {
        Challenge.insert = (fun _ ~secret_hash:_ -> challenge_error);
        find = (fun _ -> challenge_error);
        consume = (fun _ _ ~secret_hash:_ ~now:_ -> challenge_error);
        revoke = (fun _ ~now:_ -> challenge_error);
        revoke_user = (fun ?purpose:_ _ ~now:_ -> challenge_error);
        revoke_email = (fun ?purpose:_ _ ~now:_ -> challenge_error);
        gc_expired = (fun ~now:_ -> challenge_error);
      }
    in
    let passkeys =
      {
        Passkey.find = (fun _ -> None);
        list = (fun ?user_id:_ () -> []);
        insert = (fun _ -> string_error);
        update = (fun _ -> string_error);
        delete = (fun _ -> string_error);
      }
    in
    let orgs =
      {
        Org.find_org = (fun _ -> None);
        list_orgs = (fun () -> []);
        upsert_org = (fun _ -> string_error);
        delete_org = (fun _ -> string_error);
        find_membership = (fun ~org_id:_ ~user_id:_ -> None);
        list_memberships = (fun ?org_id:_ ?user_id:_ () -> []);
        upsert_membership = (fun _ -> string_error);
        delete_membership = (fun ~org_id:_ ~user_id:_ -> string_error);
        find_invite = (fun _ -> None);
        list_invites = (fun ?org_id:_ ?email:_ () -> []);
        upsert_invite = (fun _ -> string_error);
        delete_invite = (fun _ -> string_error);
      }
    in
    let mfa =
      {
        Mfa.find = (fun _ -> None);
        list = (fun ?user_id:_ ?factor:_ () -> []);
        upsert = (fun _ -> string_error);
        replace_if_current = (fun ~current:_ _ -> string_error);
        delete = (fun _ -> string_error);
      }
    in
    let scim =
      {
        Scim.find_connection = (fun _ -> None);
        list_connections = (fun ?org_id:_ () -> []);
        upsert_connection = (fun _ -> string_error);
        delete_connection = (fun _ -> string_error);
        find_user = (fun ~connection_id:_ ~external_id:_ -> None);
        list_users = (fun ?connection_id:_ () -> []);
        upsert_user = (fun ~connection_id:_ _ -> string_error);
        delete_user = (fun ~connection_id:_ ~external_id:_ -> string_error);
        find_group = (fun ~connection_id:_ ~external_id:_ -> None);
        list_groups = (fun ?connection_id:_ () -> []);
        upsert_group = (fun ~connection_id:_ _ -> string_error);
        delete_group = (fun ~connection_id:_ ~external_id:_ -> string_error);
      }
    in
    let audit =
      Audit.store ~append:(fun _ -> string_error)
        ~list:(fun ~target_user_id:_ ~org_id:_ ~kind:_ -> [])
    in
    { users; tokens; identities; challenges; passkeys; orgs; mfa; scim; audit; ensure_indexes = (fun () -> ()) }

  (* ONE collection adapter over the runtime-selected backend (minimongo / embedded Burrow / native
     mongod — all behind {!Fennec_mongo_dynamic.Dynamic}): the accounts store speaks this 5-op
     {!Collection_store.t}, and the backend choice (MONGO_URL) is invisible above here. *)
  let backend_collection (c : Backend_dyn.collection) : Collection_store.collection =
    let q selector = Backend_seam.query ~selector () in
    {
      Collection_store.find_one = (fun filter -> Backend_dyn.find_one c (q filter));
      find = (fun filter -> Backend_dyn.find c (q filter));
      insert_one = (fun doc -> try ignore (Backend_dyn.insert c doc); Ok () with exn -> Error (Printexc.to_string exn));
      update_one =
        (fun ~filter ~update ->
          try Ok (Backend_dyn.update c ~multi:false ~upsert:false filter update)
          with exn -> Error (Printexc.to_string exn));
      delete_many = (fun filter -> try Ok (Backend_dyn.remove c filter) with exn -> Error (Printexc.to_string exn));
    }

  let memory = memory_store

  (* The unified accounts store over ANY backend. [open_collection] yields a raw {!Backend_dyn} collection
     by name (the ambient [Dynamic.collection] at boot, or a test maker); each is wrapped for CRUD and the
     raw handle is kept for index DDL. usernameLower and emailsLower (the lowercased shadow fields) are
     UNIQUE + SPARSE, so absent usernames / email-less users never collide while present values stay unique
     — case-insensitively, on every backend (minimongo, Burrow, mongod). There is no per-flavor branch: the
     engine is chosen once, by MONGO_URL, inside {!Fennec_mongo_dynamic.Dynamic}. *)
  let backend ?(prefix = "accounts") ~open_collection () =
    let name suffix = prefix ^ "_" ^ suffix in
    let users_c = open_collection (name "users") in
    let tokens_c = open_collection (name "tokens") in
    let identities_c = open_collection (name "identities") in
    let challenges_c = open_collection (name "challenges") in
    let passkeys_c = open_collection (name "passkeys") in
    let orgs_c = open_collection (name "orgs") in
    let org_memberships_c = open_collection (name "org_memberships") in
    let org_invites_c = open_collection (name "org_invites") in
    let mfa_enrollments_c = open_collection (name "mfa_enrollments") in
    let scim_connections_c = open_collection (name "scim_connections") in
    let scim_users_c = open_collection (name "scim_users") in
    let scim_groups_c = open_collection (name "scim_groups") in
    let audit_c = open_collection (name "audit") in
    let ensure_indexes () =
      let idx c ~iname ~keys ?(unique = false) ?(sparse = false) () =
        Backend_dyn.ensure_index c ~name:iname ~keys ~unique ~sparse
      in
      let k1 f = Bson.doc [ (f, Bson.int 1) ] in
      (* Index the LOWERCASED shadow fields, not display [username] / nested [emails.address]: this gives
         case-insensitive uniqueness, and these top-level keys actually populate the index on minimongo and
         burrow (a dotted [emails.address] extracts no key there, so the old definition was a silent no-op
         off mongod and uniqueness rested entirely on the app-level check). Fresh index names so a re-run
         against a persistent mongod that still has the old indexes can't hit an options conflict. *)
      idx users_c ~iname:"uniq_username_lower" ~keys:(k1 "usernameLower") ~unique:true ~sparse:true ();
      idx users_c ~iname:"uniq_email_lower" ~keys:(k1 "emailsLower") ~unique:true ~sparse:true ();
      idx tokens_c ~iname:"by_user" ~keys:(k1 "userId") ();
      idx tokens_c ~iname:"by_hash" ~keys:(k1 "hashedToken") ();
      idx identities_c ~iname:"by_user" ~keys:(k1 "userId") ();
      idx identities_c ~iname:"by_stable_key" ~keys:(k1 "stableKey") ();
      idx challenges_c ~iname:"by_expiry" ~keys:(k1 "expiresAt") ();
      idx challenges_c ~iname:"by_user" ~keys:(k1 "metadata.userId") ();
      idx challenges_c ~iname:"by_email" ~keys:(k1 "metadata.email") ();
      idx passkeys_c ~iname:"by_user" ~keys:(k1 "userId") ();
      idx orgs_c ~iname:"by_domain" ~keys:(k1 "domains.name") ();
      idx org_memberships_c ~iname:"by_org" ~keys:(k1 "orgId") ();
      idx org_memberships_c ~iname:"by_user" ~keys:(k1 "userId") ();
      idx org_invites_c ~iname:"by_org" ~keys:(k1 "orgId") ();
      idx org_invites_c ~iname:"by_email" ~keys:(k1 "email") ();
      idx org_invites_c ~iname:"by_expiry" ~keys:(k1 "expiresAt") ();
      idx mfa_enrollments_c ~iname:"by_user" ~keys:(k1 "userId") ();
      idx scim_connections_c ~iname:"by_org" ~keys:(k1 "orgId") ();
      idx scim_users_c ~iname:"by_connection" ~keys:(k1 "connectionId") ();
      idx scim_groups_c ~iname:"by_connection" ~keys:(k1 "connectionId") ();
      idx audit_c ~iname:"by_target_user" ~keys:(k1 "targetUserId") ();
      idx audit_c ~iname:"by_org" ~keys:(k1 "orgId") ();
      idx audit_c ~iname:"by_kind" ~keys:(k1 "kind") ();
      idx audit_c ~iname:"by_time" ~keys:(Bson.doc [ ("at", Bson.int (-1)) ]) ()
    in
    Collection_store.make ~ensure_indexes
      {
        users = backend_collection users_c;
        tokens = backend_collection tokens_c;
        identities = backend_collection identities_c;
        challenges = backend_collection challenges_c;
        passkeys = backend_collection passkeys_c;
        orgs = backend_collection orgs_c;
        org_memberships = backend_collection org_memberships_c;
        org_invites = backend_collection org_invites_c;
        mfa_enrollments = backend_collection mfa_enrollments_c;
        scim_connections = backend_collection scim_connections_c;
        scim_users = backend_collection scim_users_c;
        scim_groups = backend_collection scim_groups_c;
        audit = backend_collection audit_c;
      }

  (* The in-process minimongo store: a fresh Minimongo per collection, needing no Eio switch — [backend]
     with a memory maker. It is the BSON-accurate reference backend for the inline tests (the same codecs
     as Burrow / native mongod); the framework reaches the engines by MONGO_URL via {!native_store}. *)
  let minimongo () = backend ~open_collection:(fun _ -> Backend_dyn.mem (Minimongo.create ())) ()

  (* annotate [t : store]: the umbrella {!Accounts_types.config} now also carries [orgs]/[passkeys]
     labels, so the projections below must be pinned to the [store] record (the last-defined wins
     otherwise). *)
  let users (t : store) = t.users
  let tokens (t : store) = t.tokens
  let identities (t : store) = t.identities
  let challenges (t : store) = t.challenges
  let passkeys (t : store) = t.passkeys
  let orgs (t : store) = t.orgs
  let mfa (t : store) = t.mfa
  let scim (t : store) = t.scim
  let audit (t : store) = t.audit
  let ensure_indexes (t : store) = t.ensure_indexes ()
end

let native_secret () =
  match Sys.getenv_opt "FENNEC_ACCOUNTS_SECRET" with
  | Some secret when String.length secret >= 16 -> secret
  | Some _ -> invalid_arg "FENNEC_ACCOUNTS_SECRET must be at least 16 bytes"
  | None -> "fennec-ephemeral-accounts-" ^ b64e (secure_random 24)

let native_store () =
  match Mongo_runtime.backend () with
  | Missing -> Store.unavailable ()
  | Memory | Burrow _ | Mongo _ ->
    (* ONE backend-blind store over the MONGO_URL-selected engine (minimongo / embedded Burrow / mongod),
       opened by name on the ambient Eio switch {!Fennec.serve} installs at boot; indexes are ensured
       idempotently. Burrow now works exactly like the rest — no special case, no "not yet wired" gap. *)
    let store = Store.backend ~open_collection:(fun name -> Backend_dyn.collection ~name ()) () in
    Store.ensure_indexes store;
    store

(* The umbrella config the native singleton is built from. [start] sets it BEFORE [boot] forces
   [current ()] (that is the order {!Fennec.serve} uses), so the singleton picks up the configured
   cookie/path/lifetime/policy/password settings — fields that are immutable on [t] and so cannot be
   retrofitted after construction. Default is {!defaults} (today's behaviour: every feature off, all
   methods on). *)
let pending_config : config Atomic.t = Atomic.make defaults

(* Install the parts of the umbrella config that the singleton's MUTABLE state can carry after it is
   built: the password/email policy, the umbrella [settings] (read by {!Accounts_http.Wiring} for the
   route table + method gate), and — when [mail] is set — the email templates the password/email verbs
   and routes need. The immutable session/policy fields are applied at construction in [make_native]. *)
let apply_settings t (cfg : config) =
  t.config <- cfg.password;
  t.settings <- cfg;
  match cfg.mail with
  | None -> ()
  | Some m ->
    let tpls =
      match m.templates with
      | Some tpls -> tpls
      | None -> Mailer.default ?site_name:m.site_name ~from:(Fennec_mail.Address.of_string m.from) ()
    in
    set_email_templates t tpls

let make_native () =
  let cfg = Atomic.get pending_config in
  let s = cfg.session in
  let t =
    make ~secret:(native_secret ()) ~store:(native_store ()) ~password_hasher:(password_hasher ())
      ?policy:cfg.rbac ~cookie:s.cookie ~path:s.path ~lifetime:s.lifetime
      ~validate_every_request:s.validate_every_request ~config:cfg.password ()
  in
  apply_settings t cfg;
  t

let current () =
  match Atomic.get native with
  | Some t -> t
  | None ->
    let t = make_native () in
    if Atomic.compare_and_set native None (Some t) then t else Option.get (Atomic.get native)

(* Apply the umbrella config to the process-native instance and auto-wire what it implies. Sets the
   pending config so a not-yet-built singleton is constructed from it; if the singleton is ALREADY
   built, applies the mutable parts in place (cookie/path/lifetime/policy can no longer change — those
   are construction-time). With the default {!defaults} this is a no-op over today's behaviour. *)
let start ?(config = defaults) () =
  Atomic.set pending_config config;
  match Atomic.get native with Some t -> apply_settings t config | None -> ()

(* ---- the config -> auto-wire seam ----------------------------------------------------------------

   [Wiring] folds the umbrella {!Accounts_types.config} held on the instance into the two things the
   framework already auto-wires: a [Paw.t] route table (consulted by {!native_paw} / {!boot}) and the
   set of DDP method names to register (consulted by the pulse server via {!method_enabled}). It is
   pure data → routes; nothing here weakens a default security posture — the enumeration guard, the
   throttle, and the verified-email gate live in the engine and apply to every wired route. The config
   only turns OPTIONAL features on: the password/email routes (when [mail] is set), the passkey routes
   (when [passkeys] is set), the SCIM battery (when [orgs.scim_prefix] is set), [GET <me_path>] (when
   set), and the authorize/callback routes for each listed provider.

   With the zero-config {!defaults} the derived table is EMPTY — so [native_paw] is exactly today's
   identity paw and the full method set stays registered. An app opts into routes by naming features. *)
module Wiring = struct
  (* join a prefix and a sub-path into one clean route path ("/auth" + "password/reset"). *)
  let join prefix sub =
    let prefix = if String.length prefix > 1 && String.ends_with ~suffix:"/" prefix then String.sub prefix 0 (String.length prefix - 1) else prefix in
    let sub = if String.length sub > 0 && sub.[0] = '/' then String.sub sub 1 (String.length sub - 1) else sub in
    if sub = "" then prefix else prefix ^ "/" ^ sub

  (* the password + email routes, mounted only when [mail] is configured. The request endpoints reuse
     the high-level send verbs (issue + deliver via the installed templates, non-enumerating); the
     consume endpoints reuse the MFA-aware [*_paw] completion constructors. Success lands on the app
     root; failures bounce back under the auth prefix — an app overrides the layout with the explicit
     [*_paw] constructors when it wants its own URLs. *)
  let mail_routes t (r : routes_config) : Paw.t list =
    let p sub = join r.auth_prefix sub in
    let success = "/" and error = join r.auth_prefix "error" in
    let mfa_required = join r.auth_prefix "mfa" in
    [
      (* request a password-reset email (non-enumerating: always redirects to success) *)
      Paw.Route.post (p "password/reset-request") (fun c ->
          match Conn.param c "email" with
          | None -> Conn.redirect c error
          | Some email -> ignore (send_reset_password_email t email); Conn.redirect c success);
      password_reset_paw t ~mfa_required ~path:(p "password/reset") ~success ~error ();
      enrollment_paw t ~mfa_required ~path:(p "password/enroll") ~success ~error ();
      (* request an email-verification email for the current user *)
      Paw.Route.post (p "email/verify-request") (fun c ->
          match user_id c with
          | None -> Conn.redirect c error
          | Some uid -> ignore (send_verification_email t uid); Conn.redirect c success);
      email_verification_paw t ~mfa_required ~path:(p "email/verify") ~success ~error ();
      (* request a magic login link / an OTP code (both naturally non-enumerating) *)
      Paw.Route.post (p "email/login-link-request") (fun c ->
          match Conn.param c "email" with
          | None -> Conn.redirect c error
          | Some email -> ignore (send_login_token_email t email); Conn.redirect c success);
      email_login_link_paw t ~mfa_required ~path:(p "email/login-link") ~success ~error ();
      Paw.Route.post (p "email/otp-request") (fun c ->
          match Conn.param c "email" with
          | None -> Conn.redirect c error
          | Some email -> ignore (send_login_token_email t email); Conn.redirect c success);
      email_otp_paw t ~mfa_required ~path:(p "email/otp") ~success ~error ();
    ]

  (* the passkey registration/assertion JSON routes, mounted only when [passkeys] is configured. *)
  let passkey_routes t (rp : Passkey.relying_party) (r : routes_config) : Paw.t list =
    let p sub = join r.auth_prefix sub in
    [
      passkey_registration_options_paw t rp ~path:(p "passkey/register/options") ();
      passkey_registration_finish_paw t rp ~path:(p "passkey/register") ();
      passkey_assertion_options_paw t rp ~path:(p "passkey/login/options") ();
      passkey_assertion_finish_paw t rp ~path:(p "passkey/login") ();
      mfa_passkey_assertion_options_paw t rp ~path:(p "passkey/mfa/options") ();
      mfa_passkey_assertion_finish_paw t rp ~path:(p "passkey/mfa") ();
    ]

  (* one provider's authorize + callback routes under [<prefix>/<id>] + [<prefix>/<id>/callback]. *)
  let provider_routes t (r : routes_config) (provider : external_identity provider) : Paw.t list =
    let p sub = join r.auth_prefix sub in
    match provider with
    | OAuth_provider { provider = pr; exchange; link_verified_email; role_map; success; error } ->
      let id = pr.OAuth.name in
      [
        oauth_authorize_paw t ~path:(p id) ~error pr ();
        oauth_callback_paw t ~link_verified_email ?role_map ~path:(p (id ^ "/callback")) ~success ~error pr ~exchange ();
      ]
    | Oidc_provider { connection; exchange; link_verified_email; role_map; success; error } ->
      let id = connection.Oidc.id in
      [
        oidc_authorize_paw t ~path:(p id) ~error connection ();
        oidc_callback_paw t ~link_verified_email ?role_map ~path:(p (id ^ "/callback")) ~success ~error connection ~exchange ();
      ]
    | Saml_provider { connection; trusted_keys; signing_key; role_map; success; error } ->
      let id = connection.Saml.id in
      [
        saml_authorize_paw t ?signing_key ~path:(p id) ~error connection ();
        saml_callback_paw t ?role_map ~path:(p (id ^ "/callback")) ~success ~error connection ~trusted_keys ();
      ]

  (* the full derived route list for an instance, in mount order. Empty for the zero-config default. *)
  let route_list t : Paw.t list =
    let cfg = t.settings in
    let r = cfg.routes in
    List.concat
      [
        (match cfg.mail with Some _ -> mail_routes t r | None -> []);
        (match cfg.passkeys with Some pk -> passkey_routes t pk.relying_party r | None -> []);
        (match cfg.orgs with Some { scim_prefix = Some prefix } -> [ scim_paw t ~prefix () ] | _ -> []);
        (match r.me_path with Some path -> [ Accounts_session.session_paw t ~path () ] | None -> []);
        List.concat_map (provider_routes t r) cfg.providers;
      ]

  (* the derived routes as one paw (first to answer wins; declines when none match). *)
  let routes t : Paw.t = Paw.seq (route_list t)

  (* the DDP method gate: today every built-in method is on. The umbrella config does not yet carry a
     method allow-list (no clean field maps to it — see the report), so this is constant [true]; the
     pulse server consults it so the gate is already in place for a later narrowing pass. *)
  let method_enabled (_t : t) (_name : string) : bool = true
end

let native_paw () : Paw.t =
 fun c ->
  let t = current () in
  (* today's identity paw FIRST (assigns user_id), THEN the config-derived routes (which read user_id).
     With the zero-config default [Wiring.routes] is the empty paw, so this is byte-identical to the
     old [paw (current ()) () c]. *)
  Paw.seq [ paw t (); Wiring.routes t ] c

(* Eagerly build the (memoized) native store at boot — inside {!Fennec.serve}'s switch, after the data
   layer's ambient switch is installed — so the engine opens and indexes are ensured BEFORE the first
   request, not lazily on it. Accounts stays incremental opt-in: an app that never authenticates pays only
   this one build, and with no MONGO_URL the store is the no-op {!Store.unavailable} — a request without a
   session cookie simply has [user_id = None]. *)
let boot () = ignore (current ())


(* ---- shared in-memory test fixture (used here and by the modules that open this one) ---- *)

let test_hasher =
  Password.
    {
    hash = (fun ~password -> "test$" ^ password);
    verify = (fun ~password ~hash -> hash = "test$" ^ password);
    }

let test_accounts () = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher ()

let test_verified_email_key raw =
  match Identity.email ~verified:true raw with
  | Ok key -> key
  | Error e -> failwith (Identity.string_of_error e)

let test_active_totp user_id =
  match
    Mfa.enrollment ~now:(fun () -> 10.) ~status:Mfa.Active ~id:("mfa-" ^ user_id) ~user_id
      ~factor:Mfa.Totp ~secret:"SECRET" ~confirmed_at:10. ()
  with
  | Ok enrollment -> enrollment
  | Error e -> failwith (Mfa.string_of_error e)

let identity_ok = function Ok x -> x | Error e -> failwith (Identity.string_of_error e)


let%test "memory_store update rejects duplicate usernames" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match (create_user a ~username:"ada" (), create_user a ~username:"bob" ()) with
  | Ok _, Ok bob -> (
    match store.users.update_user { bob with username = Some "ADA" } with Error (Duplicate_username "ada") -> true | _ -> false)
  | _ -> false
let%test "memory_store update rejects duplicate services on the same user" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok u -> (
    match
      store.users.update_user
        { u with services = [ ("github", Bson.doc [ ("id", Bson.str "1") ]); ("github", Bson.doc [ ("id", Bson.str "2") ]) ] }
    with
    | Error (Invalid_user "Duplicate service: github") -> true
    | _ -> false)
let%test "Store.minimongo supports password login and epoch revocation" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"Ada" ~email:"ADA@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match login_with_password a (By_email "ada@example.com") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      Store.ensure_indexes store;
      logout_other_clients a user.id = Ok ()
      && verify_token a token = Error Invalid_token
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw"))
(* C1 regression guard: after switching the unified store from full-collection scans to indexed lookups on
   the lowercased shadow fields, login must still be case-insensitive by BOTH email and username, the display
   username must stay case-preserving, and case-variant duplicates must still be rejected. Exercises the
   built indexes (ensure_indexes) on the minimongo-backed Collection_store — the same code path Burrow/mongod
   take. *)
let%test "unified store: case-insensitive email + username login via the indexed shadow fields" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  Store.ensure_indexes store;
  match create_user a ~username:"Ada" ~email:"ADA@Example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok user ->
    (* display case preserved; email stored normalized *)
    user.username = Some "Ada"
    && List.exists (fun e -> e.address = "ada@example.com") user.emails
    (* login resolves regardless of the case the caller types, by email AND username *)
    && Result.is_ok (login_with_password a (By_email "ada@example.com") ~password:"pw")
    && Result.is_ok (login_with_password a (By_email "ADA@EXAMPLE.COM") ~password:"pw")
    && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw")
    && Result.is_ok (login_with_password a (By_username "ADA") ~password:"pw")
    (* the indexed lookup resolves to the very same user the id lookup does *)
    && (match find_by_selector a (By_email "aDa@example.COM") with Ok (Some u) -> u.id = user.id | _ -> false)
    && (match find_by_selector a (By_username "aDa") with Ok (Some u) -> u.id = user.id | _ -> false)
    (* a case-variant of an existing email / username is rejected — case-insensitive uniqueness *)
    && (match create_user a ~email:"ada@EXAMPLE.com" () with Error (Duplicate_email _) -> true | _ -> false)
    && (match create_user a ~username:"ADA" () with Error (Duplicate_username _) -> true | _ -> false)
(* Email-less users carry no [emailsLower] (sparse-index safety), so removing a user's last email must
   $unset the shadow rather than leave it stale — otherwise the indexed lookup/uniqueness would still match a
   gone address. Proves the address both stops resolving AND frees up for a new account. *)
let%test "unified store: removing the last email clears the indexed shadow (no stale match, address frees up)" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  Store.ensure_indexes store;
  match create_user a ~username:"bob" ~email:"bob@example.com" () with
  | Error _ -> false
  | Ok user -> (
    match remove_email a user.id "bob@example.com" with
    | Error _ -> false
    | Ok _ ->
      find_by_selector a (By_email "bob@example.com") = Ok None
      && Result.is_ok (create_user a ~username:"carol" ~email:"BOB@example.com" ()))
let%test "Store.unavailable lets the framework boot but fails account writes clearly" =
  let store = Store.unavailable ~message:"no mongo configured" () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~email:"ada@example.com" () with
  | Error (Store_error msg) -> msg = "no mongo configured"
  | _ -> false
let%test "Store.minimongo challenge facet is single-use" =
  let store = Store.minimongo () in
  let challenges =
    Challenge.make ~secret:"accounts-challenge-secret" ~store:(Store.challenges store) ~ttl:60. ()
  in
  match Challenge.create challenges ~purpose:Challenge.Email_login () with
  | Error _ -> false
  | Ok issued ->
    Result.is_ok (Challenge.consume challenges ~purpose:Challenge.Email_login issued.token)
    && Challenge.consume challenges ~purpose:Challenge.Email_login issued.token = Error Challenge.Already_consumed
let%test "Store.minimongo audit facet appends and filters events" =
  let store = Store.minimongo () in
  let audit = Store.audit store in
  let a = Audit.event ~id:"evt-1" ~at:1. ~target_user_id:"u1" ~org_id:"org" Audit.Login Audit.Anonymous Audit.Success in
  let b = Audit.event ~id:"evt-2" ~at:2. ~target_user_id:"u2" ~org_id:"org" Audit.Logout Audit.Anonymous Audit.Success in
  Audit.append audit a = Ok ()
  && Audit.append audit b = Ok ()
  && Result.is_error (Audit.append audit a)
  && Audit.list ~target_user_id:"u1" audit = [ a ]
  && Audit.list ~org_id:"org" audit = [ a; b ]
  && Audit.list ~kind:Audit.Logout audit = [ b ]
let%test "Store.minimongo org facet persists org membership and invite documents" =
  let store = Store.minimongo () in
  let orgs = Store.orgs store in
  let domain = match Org.domain ~verified:true "example.com" with Ok d -> d | Error _ -> assert false in
  let org = match Org.org ~id:"acme" ~name:"Acme" ~domains:[ domain ] () with Ok o -> o | Error _ -> assert false in
  let membership =
    match Org.membership ~now:(fun () -> 10.) ~org_id:"acme" ~user_id:"u1" ~role:"admin" () with
    | Ok membership -> membership
    | Error _ -> assert false
  in
  let invite =
    match
      Org.invite ~now:(fun () -> 20.) ~id:"inv1" ~org_id:"acme" ~email:"Ada@Example.com" ~role:"member"
        ~token_hash:"hash" ()
    with
    | Ok invite -> invite
    | Error _ -> assert false
  in
  orgs.upsert_org org = Ok ()
  && orgs.upsert_membership membership = Ok ()
  && orgs.upsert_invite invite = Ok ()
  && orgs.find_org "acme" = Some org
  && orgs.find_membership ~org_id:"acme" ~user_id:"u1" = Some membership
  && orgs.list_invites ~email:"ada@example.com" () = [ invite ]
let%test "Store.minimongo mfa facet persists enrollments" =
  let store = Store.minimongo () in
  let mfa = Store.mfa store in
  let enrollment =
    match
      Mfa.enrollment ~now:(fun () -> 10.) ~status:Mfa.Active ~id:"mfa1" ~user_id:"u1" ~factor:Mfa.Totp
        ~secret:"SECRET" ~last_step:1L ~confirmed_at:11. ()
    with
    | Ok enrollment -> enrollment
    | Error _ -> assert false
  in
  let next = { enrollment with Mfa.last_step = Some 2L } in
  let stale = { enrollment with Mfa.last_step = Some 3L } in
  mfa.upsert enrollment = Ok ()
  && mfa.find "mfa1" = Some enrollment
  && mfa.replace_if_current ~current:enrollment next = Ok true
  && mfa.replace_if_current ~current:enrollment stale = Ok false
  && mfa.find "mfa1" = Some next
  && mfa.list ~user_id:"u1" ~factor:Mfa.Totp () = [ next ]
let%test "Store.minimongo scim facet persists connection users and groups" =
  let store = Store.minimongo () in
  let scim = Store.scim store in
  let connection =
    match Scim.connection ~id:"corp" ~org_id:"acme" ~bearer_token:"very-secret-scim-token" () with
    | Ok connection -> connection
    | Error _ -> assert false
  in
  let user =
    match Scim.user ~external_id:"u1" ~user_name:"ada" ~emails:[ "Ada@example.com" ] () with
    | Ok user -> user
    | Error _ -> assert false
  in
  let group =
    match Scim.group ~external_id:"g1" ~display_name:"Admins" ~members:[ "u1" ] () with
    | Ok group -> group
    | Error _ -> assert false
  in
  scim.upsert_connection connection = Ok ()
  && scim.upsert_user ~connection_id:connection.id user = Ok ()
  && scim.upsert_group ~connection_id:connection.id group = Ok ()
  && scim.find_connection "corp" = Some connection
  && scim.find_user ~connection_id:"corp" ~external_id:"u1" = Some user
  && scim.find_group ~connection_id:"corp" ~external_id:"g1" = Some group

(* ---- shared HTTP / provider test fixtures (used by the methods + integration suites) ---- *)

let test_passkey_credential ?(id = "cred-1") user_id =
  Mirage_crypto_rng_unix.use_default ();
  {
    Passkey.id;
    user_id;
    user_handle = "handle-" ^ user_id;
    public_key = X509.Private_key.public (X509.Private_key.generate `P256);
    sign_count = 1l;
    backup_eligible = false;
    backed_up = false;
    transports = [ "internal" ];
    created_at = 1_000.;
    last_used_at = None;
  }
let same_passkey_credential (a : Passkey.credential) (b : Passkey.credential) =
  a.id = b.id && a.user_id = b.user_id && a.user_handle = b.user_handle
  && X509.Public_key.encode_pem a.public_key = X509.Public_key.encode_pem b.public_key
  && a.sign_count = b.sign_count
  && a.backup_eligible = b.backup_eligible
  && a.backed_up = b.backed_up
  && a.transports = b.transports
  && a.created_at = b.created_at
  && a.last_used_at = b.last_used_at
let test_email_helper () =
  let challenge =
    Challenge.make ~secret:"accounts-email-challenge-secret" ~store:(Challenge.memory_store ()) ()
  in
  Email.make ~secret:"accounts-email-helper-secret" ~challenge
let req_ ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ()
let post_form_ path body =
  H.make_request ~meth:H.POST ~path ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body ()
let finalize_ c = Conn.apply_before_send c (Option.get (Conn.resp c))
let cookie_kv_ set_cookie =
  match String.index_opt set_cookie ';' with Some i -> String.sub set_cookie 0 i | None -> set_cookie
let location_ r = Paw.Headers.get r.H.headers "location"
let url_path_ url =
  match String.index_opt url '?' with
  | None -> url
  | Some i -> String.sub url 0 i
let url_query_ url =
  match String.index_opt url '?' with
  | None -> []
  | Some i -> H.parse_query (String.sub url (i + 1) (String.length url - i - 1))
let mfa_redirect_ok_ a user_id r =
  match location_ r with
  | None -> false
  | Some url -> (
    let params = url_query_ url in
    url_path_ url = "/mfa"
    && List.assoc_opt "userId" params = Some user_id
    &&
    match List.assoc_opt "mfaToken" params with
    | None -> false
    | Some token ->
      Result.is_ok (Mfa.consume_step_up (mfa_service a) ~expected_user:user_id (Challenge.token_of_string token)))
let passkey_rp_ () =
  match Passkey.relying_party ~id:"app.test" ~name:"App" ~origins:[ "https://app.test" ] () with
  | Ok rp -> rp
  | Error e -> failwith (Passkey.string_of_error e)
let query_param_ url name =
  match String.index_opt url '?' with
  | None -> None
  | Some i ->
    let query = String.sub url (i + 1) (String.length url - i - 1) in
    List.assoc_opt name (H.parse_query query)
let oauth_provider_ () =
  match
    OAuth.provider ~name:"github" ~authorize_url:"https://github.test/authorize" ~client_id:"client"
      ~redirect_uri:"https://app.test/oauth/callback" ()
  with
  | Ok provider -> provider
  | Error e -> failwith (OAuth.string_of_error e)
let oidc_connection_ () =
  match
    Oidc.connection ~id:"main" ~issuer:"https://idp.test" ~authorize_url:"https://idp.test/auth"
      ~client_id:"client" ~redirect_uri:"https://app.test/oidc/callback" ~allow_jit:true ()
  with
  | Ok connection -> connection
  | Error e -> failwith (Oidc.string_of_error e)

let saml_connection_ () =
  match
    Saml.connection ~id:"okta" ~issuer:"https://idp.test" ~sso_url:"https://idp.test/sso"
      ~entity_id:"sp" ~acs_url:"https://app.test/auth/okta/callback" ()
  with
  | Ok connection -> connection
  | Error e -> failwith (Saml.string_of_error e)

(* ---- inline tests: the config -> route-table derivation (Wiring) ----------------------------------

   The proof the auto-wire seam is correct and the default path is preserved: zero-config derives an
   EMPTY route table (so native_paw is exactly today's identity paw); naming a feature mounts exactly
   that feature's routes and nothing else; a narrowed config omits the rest; the prefix relocates them. *)

(* an instance whose umbrella [settings] is [cfg] (test_accounts builds with [defaults]). *)
let wired_ cfg = let a = test_accounts () in a.settings <- cfg; a

(* is [req] answered by the config-derived route table? (an unmounted path falls through Paw.seq [] →
   404; every mounted accounts route answers with a redirect / JSON / 401, never a 404). *)
let route_mounted_ a req = (Paw.run (Wiring.routes a) req).H.status <> 404
let get_ path = H.make_request ~meth:H.GET ~path ()
let post_ path = H.make_request ~meth:H.POST ~path ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"" ()

(* the canonical probe paths per feature, under the default "/auth" prefix *)
let mail_probe_ = post_ "/auth/password/reset-request"
let passkey_probe_ = get_ "/auth/passkey/login/options"
let scim_probe_ = get_ "/scim/v2/ServiceProviderConfig"
let me_probe_ = get_ "/me"
let oauth_authorize_probe_ = get_ "/auth/github"
let oauth_callback_probe_ = get_ "/auth/github/callback"

let%test "Wiring: zero-config derives an EMPTY route table (today's behaviour preserved)" =
  let a = wired_ defaults in
  (not (route_mounted_ a mail_probe_))
  && (not (route_mounted_ a passkey_probe_))
  && (not (route_mounted_ a scim_probe_))
  && (not (route_mounted_ a me_probe_))
  && (not (route_mounted_ a oauth_authorize_probe_))
  (* the empty derived paw declines every path *)
  && (Paw.run (Wiring.routes a) (get_ "/anything")).H.status = 404

let%test "Wiring: mail config mounts the password/email routes and nothing else" =
  let a = wired_ { defaults with mail = Some { from = "no-reply@acme.test"; site_name = Some "Acme"; templates = None } } in
  route_mounted_ a mail_probe_
  && route_mounted_ a (post_ "/auth/email/otp-request")
  && route_mounted_ a (get_ "/auth/email/verify")
  && (not (route_mounted_ a passkey_probe_))
  && (not (route_mounted_ a scim_probe_))
  && (not (route_mounted_ a oauth_authorize_probe_))

let%test "Wiring: passkeys config mounts the passkey routes and not the mail routes" =
  let a = wired_ { defaults with passkeys = Some { relying_party = passkey_rp_ () } } in
  route_mounted_ a passkey_probe_
  && route_mounted_ a (get_ "/auth/passkey/register/options")
  && route_mounted_ a (get_ "/auth/passkey/mfa/options")
  && (not (route_mounted_ a mail_probe_))
  && (not (route_mounted_ a scim_probe_))

let%test "Wiring: orgs.scim_prefix mounts the SCIM battery at that prefix" =
  let a = wired_ { defaults with orgs = Some { scim_prefix = Some "/scim/v2" } } in
  route_mounted_ a scim_probe_
  && route_mounted_ a (get_ "/scim/v2/Users")
  (* not at the default-ish other prefix; not the mail/passkey routes *)
  && (not (route_mounted_ a (get_ "/scim/ServiceProviderConfig")))
  && (not (route_mounted_ a mail_probe_))

let%test "Wiring: me_path mounts GET <me_path> only when set" =
  let off = wired_ defaults in
  let on = wired_ { defaults with routes = { defaults.routes with me_path = Some "/me" } } in
  (not (route_mounted_ off me_probe_)) && route_mounted_ on me_probe_

let%test "Wiring: an OAuth provider mounts authorize + callback under <prefix>/<id>" =
  let provider =
    OAuth_provider
      { provider = oauth_provider_ (); exchange = (fun _ ~code:_ -> Error (Login_rejected "stub")); link_verified_email = true; role_map = None; success = "/"; error = "/auth/error" }
  in
  let a = wired_ { defaults with providers = [ provider ] } in
  route_mounted_ a oauth_authorize_probe_
  && route_mounted_ a oauth_callback_probe_
  (* a provider does NOT pull in the mail/passkey/scim routes *)
  && (not (route_mounted_ a mail_probe_))
  && (not (route_mounted_ a passkey_probe_))

let%test "Wiring: an OIDC + a SAML provider each mount their own authorize + callback" =
  let oidc = Oidc_provider { connection = oidc_connection_ (); exchange = (fun _ ~code:_ -> Error (Login_rejected "stub")); link_verified_email = true; role_map = None; success = "/"; error = "/auth/error" } in
  let saml = Saml_provider { connection = saml_connection_ (); trusted_keys = []; signing_key = None; role_map = None; success = "/"; error = "/auth/error" } in
  let a = wired_ { defaults with providers = [ oidc; saml ] } in
  route_mounted_ a (get_ "/auth/main")
  && route_mounted_ a (get_ "/auth/main/callback")
  && route_mounted_ a (get_ "/auth/okta")
  && route_mounted_ a (post_ "/auth/okta/callback")

let%test "Wiring: a custom auth_prefix relocates the derived routes" =
  let a = wired_ { defaults with mail = Some { from = "no-reply@acme.test"; site_name = None; templates = None }; routes = { auth_prefix = "/identity"; me_path = None } } in
  route_mounted_ a (post_ "/identity/password/reset-request")
  (* the default "/auth" location is now empty *)
  && (not (route_mounted_ a mail_probe_))

let%test "Wiring: a narrowed config (mail only) omits passkey/scim/provider routes" =
  let a = wired_ { defaults with mail = Some { from = "no-reply@acme.test"; site_name = None; templates = None } } in
  route_mounted_ a mail_probe_
  && (not (route_mounted_ a passkey_probe_))
  && (not (route_mounted_ a scim_probe_))
  && (not (route_mounted_ a oauth_authorize_probe_))
  && (not (route_mounted_ a oauth_callback_probe_))

(* the defaults value + sub-record round-trips: a one-field override leaves everything else at the
   secure/empty defaults, and [default_config] still aliases [defaults.password]. *)
let%test "defaults: zero-config is all-off with secure password defaults" =
  defaults.mail = None && defaults.passkeys = None && defaults.orgs = None && defaults.rbac = None
  && defaults.providers = []
  && defaults.routes.auth_prefix = "/auth"
  && defaults.routes.me_path = None
  && defaults.session.cookie = "_fennec_login"
  && defaults.session.lifetime = 86_400.
  && defaults.password.ambiguous_error_messages = true
  && defaults.password == default_config

let%test "defaults: a one-field override is a one-liner that leaves the rest at defaults" =
  let cfg = { defaults with mail = Some { from = "x@y.z"; site_name = Some "Acme"; templates = None } } in
  cfg.passkeys = None && cfg.orgs = None && cfg.providers = []
  && cfg.session = defaults.session
  && cfg.password = defaults.password
  && (match cfg.mail with Some m -> m.from = "x@y.z" && m.site_name = Some "Acme" | None -> false)

(* the method gate is currently "every method on" — the default path keeps the full DDP method set. *)
let%test "Wiring.method_enabled: every built-in method is enabled by default" =
  let a = wired_ defaults in
  List.for_all (Wiring.method_enabled a)
    [ "createUser"; "currentUser"; "login"; "logout"; "logoutOtherClients"; "changePassword";
      "resetPassword"; "verifyEmail"; "enrollAccount"; "completeLoginStepUp"; "forgotPassword"; "requestLoginToken" ]
