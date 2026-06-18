(* accounts_profile.ml — the OPTIONAL Sift-typed view of [user.profile] (Stage 3c).

   [user.profile] stays exactly what it has always been: an opaque [Bson.t option] the framework never
   interprets (application-owned data). This module is a thin, additive convenience for apps that DO want
   a typed profile: describe the profile shape once as a ['p Sift.t] codec, and read/write it through
   that codec instead of hand-rolling BSON. It changes no storage: a typed profile is encoded to the same
   [Bson.t] that lands in the [profile] slot, and reads back through [Sift.decode].

   Fail-safe by design: [typed_profile] returns [None] for an absent profile OR a profile that does not
   match the codec (a foreign shape, a partial write, a schema drift) — it never raises and never returns
   a half-decoded value. An app that needs to distinguish "absent" from "present-but-wrong-shape" uses
   {!typed_profile_result}. *)

open Accounts_base

(* The typed-profile handle: a ['p Sift.t] codec paired with the phantom, so [typed_profile] /
   [set_typed_profile] stay concrete at use sites. It is just the codec — building one is free and holds
   no Accounts state, so the same handle works against any instance / user. *)
type 'p profile_codec = 'p Sift.t

let profile_codec (codec : 'p Sift.t) : 'p profile_codec = codec

(* Decode [user.profile] through the codec, surfacing the decode errors. [Ok None] = no profile on the
   user; [Ok (Some p)] = a profile that matched; [Error es] = a profile present but not of this shape. *)
let typed_profile_result (codec : 'p profile_codec) (user : user) : ('p option, Sift.error list) result =
  match user.profile with
  | None -> Ok None
  | Some bson -> ( match Sift.decode codec bson with Ok p -> Ok (Some p) | Error es -> Error es)

(* The fail-safe reader: an absent profile AND a foreign / unparseable profile both read as [None], so a
   caller can never trip over another shape's data sitting in the slot. *)
let typed_profile (codec : 'p profile_codec) (user : user) : 'p option =
  match typed_profile_result codec user with Ok p -> p | Error _ -> None

(* Encode a typed value to the BSON profile slot and persist it through the ordinary [set_profile] write
   (same uniqueness / audit / store path as a raw profile write). [Sift.to_bson] is total — a typed value
   always serializes — so this only fails for the usual store reasons ([User_not_found] / [Store_error]). *)
let set_typed_profile (t : t) (codec : 'p profile_codec) (uid : user_id) (value : 'p) : (user, error) result =
  set_profile t uid (Some (Sift.to_bson codec value))

(* clear the profile (the [None] write), for symmetry with [set_typed_profile]. *)
let clear_profile (t : t) (uid : user_id) : (user, error) result = set_profile t uid None

(* ---- inline tests: round-trip + fail-safe (against the in-memory store) ----------------------------

   The fixture lives in {!Accounts_native} (it needs [make] + an assembled store, both above the engine);
   this module sits above {!Accounts_native}, so the tests open it for [test_accounts] / [memory_store]. *)
open Accounts_native [@@warning "-33"]

(* a small typed profile shape: { display_name : string; theme : string; age : int }. *)
type demo_profile = { display_name : string; theme : string; age : int }

let demo_codec : demo_profile profile_codec =
  profile_codec
    Sift.(
      record (fun display_name theme age -> { display_name; theme; age })
      |> field (req "displayName" string) (fun p -> p.display_name)
      |> field (req "theme" string) (fun p -> p.theme)
      |> field (req "age" int) (fun p -> p.age)
      |> seal)

let%test "typed profile round-trips through set_typed_profile / typed_profile" =
  let a = test_accounts () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    let value = { display_name = "Ada Lovelace"; theme = "dark"; age = 36 } in
    match set_typed_profile a demo_codec user.id value with
    | Error _ -> false
    | Ok user' -> (
      (* the stored slot is real BSON the framework still treats as opaque *)
      (match user'.profile with Some _ -> true | None -> false)
      (* and it decodes back to the exact value *)
      && typed_profile demo_codec user' = Some value
      && typed_profile_result demo_codec user' = Ok (Some value)))

let%test "a user with no profile reads as None (fail-safe, not an error)" =
  let a = test_accounts () in
  match create_user a ~username:"bob" () with
  | Error _ -> false
  | Ok user -> typed_profile demo_codec user = None && typed_profile_result demo_codec user = Ok None

let%test "a FOREIGN / wrong-shape profile reads as None, and the result form reports the errors" =
  let a = test_accounts () in
  match create_user a ~username:"carol" () with
  | Error _ -> false
  | Ok user -> (
    (* write a profile that is NOT the demo shape (missing every required field) *)
    match set_profile a user.id (Some (Bson.doc [ ("unrelated", Bson.str "value") ])) with
    | Error _ -> false
    | Ok user' ->
      (* fail-safe: the reader yields None rather than a partial / raised value *)
      typed_profile demo_codec user' = None
      (* and the explicit form surfaces a non-empty error list (so an app CAN tell absent from foreign) *)
      && (match typed_profile_result demo_codec user' with Error (_ :: _) -> true | _ -> false))

let%test "clear_profile removes a previously-set typed profile" =
  let a = test_accounts () in
  match create_user a ~username:"dave" () with
  | Error _ -> false
  | Ok user -> (
    match set_typed_profile a demo_codec user.id { display_name = "Dave"; theme = "light"; age = 20 } with
    | Error _ -> false
    | Ok _ -> (
      match clear_profile a user.id with
      | Error _ -> false
      | Ok user' -> user'.profile = None && typed_profile demo_codec user' = None))
