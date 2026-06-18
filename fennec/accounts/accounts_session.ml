(* accounts_session.ml — the safe session-view BSON serializers for the HTTP surface: the
   [public_user_to_doc] / [auth_context_to_doc] / [assurance_to_doc] / [org_context_to_doc]
   field encoders, and the [session_doc] / [session_paw] that render the current account (user +
   auth context + assurance + org, never any secret material) for a [/me]-style endpoint.

   Distinct from {!Accounts_methods}'s DDP [ddp_session_doc] (a leaner, DDP-shaped payload): this is
   the richer HTTP view. It sits above {!Accounts_codec} (it reuses the record codecs), so unlike the
   pure HTTP routes in {!Accounts_http} — which are part of {!Accounts_base}, compiled before the
   codec — it lives here, one layer up, alongside the facade's [Codec] alias. The facade [include]s
   this module, so [session_doc] / [session_paw] are exported at the [Accounts] top level unchanged. *)

open Accounts_base
module Codec = Accounts_codec

let mfa_level_to_string = function
  | Mfa.Anonymous -> "anonymous"
  | Single_factor -> "single_factor"
  | Phishing_resistant_single_factor -> "phishing_resistant_single_factor"
  | Multi_factor -> "multi_factor"
  | Phishing_resistant_multi_factor -> "phishing_resistant_multi_factor"

(* [opt_bson] (None -> Bson.null) is defined once in Accounts_types and reaches here via
   [open Accounts_base]. *)

let public_user_to_doc (user : user) =
  let fields =
    [
      ("_id", Bson.str user.id);
      ("id", Bson.str user.id);
      ("emails", Bson.array (List.map Codec.email_to_doc user.emails));
      ("roles", Bson.array (List.map (fun role -> Bson.str (Roles.Role.name role)) user.roles));
      ("status", Bson.str (string_of_user_status user.status));
      ("createdAt", Bson.float user.created_at);
      ("updatedAt", Bson.float user.updated_at);
    ]
  in
  let fields =
    match user.username with Some username -> ("username", Bson.str username) :: fields | None -> fields
  in
  let fields = match user.profile with Some profile -> ("profile", profile) :: fields | None -> fields in
  Bson.doc (List.rev fields)

let auth_context_to_doc (ctx : auth_context) =
  Bson.doc
    [
      ("userId", Bson.str ctx.user_id);
      ("sessionId", Bson.str ctx.session_id);
      ("strategy", Bson.str ctx.strategy);
      ("factors", Bson.array (List.map (fun factor -> Bson.str (Codec.mfa_factor_to_string factor)) ctx.factors));
      ("issuedAt", Bson.float ctx.issued_at);
      ("expiresAt", Bson.float ctx.expires_at);
      ("authEpoch", Bson.int ctx.auth_epoch);
    ]

let assurance_to_doc (assurance : Mfa.assurance) =
  Bson.doc
    [
      ("level", Bson.str (mfa_level_to_string assurance.level));
      ("factors", Bson.array (List.map (fun factor -> Bson.str (Codec.mfa_factor_to_string factor)) assurance.factors));
      ("authenticatedAt", Bson.float assurance.authenticated_at);
    ]

let org_context_to_doc (ctx : org_context) =
  Bson.doc
    [
      ("org", Codec.org_to_doc ctx.org);
      ("membership", opt_bson (Option.map Codec.membership_to_doc ctx.membership));
    ]

let session_doc t c =
  Result.map
    (fun user ->
      Bson.doc
        [
          ("userId", opt_bson (Option.map (fun user -> Bson.str user.id) user));
          ("user", opt_bson (Option.map public_user_to_doc user));
          ("authContext", opt_bson (Option.map auth_context_to_doc (auth_context c)));
          ("assurance", opt_bson (Option.map assurance_to_doc (assurance c)));
          ("org", opt_bson (Option.map org_context_to_doc (org_context c)));
        ])
    (current_user t c)

let session_paw t ~path () =
  Paw.Route.get path (fun c ->
      let c = paw t () c in
      match session_doc t c with
      | Ok doc -> Conn.json ~headers:[ ("Cache-Control", "no-store") ] c (Bson_json.to_string doc)
      | Error e ->
        Conn.json ~status:500 ~headers:[ ("Cache-Control", "no-store") ] c
          (Bson_json.to_string (Bson.doc [ ("error", Bson.str (string_of_error e)) ])))

