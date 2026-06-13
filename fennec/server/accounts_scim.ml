module Identity = Accounts_identity
module Org = Accounts_org

type error =
  | Invalid_connection of string
  | Invalid_token
  | Invalid_user of string
  | Invalid_group of string
  | Invalid_patch of string
  | Identity_error of Identity.error
  | Org_error of Org.error

let string_of_error = function
  | Invalid_connection s -> "Invalid SCIM connection: " ^ s
  | Invalid_token -> "Invalid SCIM bearer token"
  | Invalid_user s -> "Invalid SCIM user: " ^ s
  | Invalid_group s -> "Invalid SCIM group: " ^ s
  | Invalid_patch s -> "Invalid SCIM patch: " ^ s
  | Identity_error e -> Identity.string_of_error e
  | Org_error e -> Org.string_of_error e

type connection = {
  id : string;
  org_id : string;
  token_hash : string;
  allow_deprovision : bool;
  default_role : string;
}

type user = {
  id : string option;
  external_id : string;
  user_name : string;
  active : bool;
  emails : string list;
  display_name : string option;
  groups : string list;
}

type group = {
  id : string option;
  external_id : string;
  display_name : string;
  members : string list;
}

type user_path =
  | User_name
  | Active
  | Emails
  | Display_name
  | Groups
  | External_id

type patch_op =
  | Add of user_path * string list
  | Replace of user_path * string list
  | Remove of user_path * string list

type group_path =
  | Group_display_name
  | Group_members
  | Group_external_id

type group_patch_op =
  | Group_add of group_path * string list
  | Group_replace of group_path * string list
  | Group_remove of group_path * string list

type user_plan =
  | No_user_change
  | Create_user of user
  | Update_user of {
      before : user;
      after : user;
    }
  | Deprovision_user of {
      before : user;
      after : user;
    }

type group_plan =
  | No_group_change
  | Create_group of group
  | Update_group of {
      before : group;
      after : group;
    }
  | Delete_group of group

type membership_delta = {
  add : string list;
  remove : string list;
}

let trim = String.trim
let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))

let constant_eq a b =
  let la = String.length a and lb = String.length b in
  let diff = ref (la lxor lb) in
  let max_len = max la lb in
  for i = 0 to max_len - 1 do
    let ca = if i < la then Char.code a.[i] else 0 in
    let cb = if i < lb then Char.code b.[i] else 0 in
    diff := !diff lor (ca lxor cb)
  done;
  !diff = 0

let normalize_id_error = function
  | Ok id -> Ok id
  | Error e -> Error (Org_error e)

let normalize_role_error = function
  | Ok role -> Ok role
  | Error e -> Error (Org_error e)

let normalize_external_id kind raw =
  let value = trim raw in
  if value = "" then Error (kind "external_id cannot be blank") else Ok value

let normalize_optional_id raw =
  match Option.map trim raw with
  | Some "" | None -> None
  | Some id -> Some id

let normalize_user_name raw =
  let value = trim raw in
  if value = "" then Error (Invalid_user "userName cannot be blank") else Ok value

let normalize_display_name raw =
  match Option.map trim raw with
  | Some "" | None -> None
  | Some name -> Some name

let valid_email email =
  match String.index_opt email '@' with
  | None -> false
  | Some 0 -> false
  | Some i when i = String.length email - 1 -> false
  | Some _ ->
    not
      (String.exists
         (function
           | ' ' | '\t' | '\n' | '\r' -> true
           | _ -> false)
         email)

let normalize_email raw =
  let email = String.lowercase_ascii (trim raw) in
  if email = "" then Error (Invalid_user "email cannot be blank")
  else if valid_email email then Ok email
  else Error (Invalid_user ("invalid email: " ^ raw))

let uniq_sorted values = List.sort_uniq String.compare values

let normalize_emails emails =
  let rec loop acc = function
    | [] -> Ok (uniq_sorted acc)
    | email :: rest -> (
      match normalize_email email with
      | Ok email -> loop (email :: acc) rest
      | Error _ as e -> e)
  in
  loop [] emails

let normalize_ref_list kind values =
  let rec loop acc = function
    | [] -> Ok (uniq_sorted acc)
    | value :: rest ->
      let value = trim value in
      if value = "" then Error (kind "blank reference")
      else loop (value :: acc) rest
  in
  loop [] values

let connection ?(allow_deprovision = true) ?(default_role = "member") ~id ~org_id ~bearer_token () =
  match (normalize_id_error (Org.normalize_id id), normalize_id_error (Org.normalize_id org_id), normalize_role_error (Org.normalize_role default_role)) with
  | Error e, _, _ -> Error e
  | _, Error e, _ -> Error e
  | _, _, Error e -> Error e
  | Ok id, Ok org_id, Ok default_role ->
    let bearer_token = trim bearer_token in
    if String.length bearer_token < 16 then Error (Invalid_connection "bearer_token must be at least 16 bytes")
    else Ok { id; org_id; token_hash = sha256_hex bearer_token; allow_deprovision; default_role }

let authenticate connection ~bearer_token =
  if constant_eq connection.token_hash (sha256_hex (trim bearer_token)) then Ok () else Error Invalid_token

let user ?id ?(active = true) ?(emails = []) ?display_name ?(groups = []) ~external_id ~user_name () =
  match (normalize_external_id (fun s -> Invalid_user s) external_id, normalize_user_name user_name, normalize_emails emails, normalize_ref_list (fun s -> Invalid_user s) groups) with
  | Error e, _, _, _ -> Error e
  | _, Error e, _, _ -> Error e
  | _, _, Error e, _ -> Error e
  | _, _, _, Error e -> Error e
  | Ok external_id, Ok user_name, Ok emails, Ok groups ->
    Ok { id = normalize_optional_id id; external_id; user_name; active; emails; display_name = normalize_display_name display_name; groups }

let group ?id ?(members = []) ~external_id ~display_name () =
  match (normalize_external_id (fun s -> Invalid_group s) external_id, normalize_ref_list (fun s -> Invalid_group s) members) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok external_id, Ok members ->
    let display_name = trim display_name in
    if display_name = "" then Error (Invalid_group "displayName cannot be blank")
    else Ok { id = normalize_optional_id id; external_id; display_name; members }

let identity (connection : connection) (user : user) =
  match Identity.scim ~org_id:connection.org_id ~external_id:user.external_id with
  | Ok key -> Ok key
  | Error e -> Error (Identity_error e)

let membership ?now (connection : connection) ~user_id (user : user) =
  let status = if user.active then Org.Active_member else Org.Disabled in
  match Org.membership ?now ~status ~role:connection.default_role ~external_id:user.external_id ~org_id:connection.org_id ~user_id () with
  | Ok membership -> Ok membership
  | Error e -> Error (Org_error e)

let bool_of_values = function
  | [ value ] -> (
    match String.lowercase_ascii (trim value) with
    | "true" -> Ok true
    | "false" -> Ok false
    | _ -> Error (Invalid_patch "active expects true or false"))
  | _ -> Error (Invalid_patch "active expects one value")

let one_value field = function
  | [ value ] when trim value <> "" -> Ok (trim value)
  | [ _ ] -> Error (Invalid_patch (field ^ " cannot be blank"))
  | _ -> Error (Invalid_patch (field ^ " expects one value"))

let apply_user_patch user ops =
  let replace_list normalize values =
    normalize values
  in
  let add_list current normalize values =
    match normalize values with
    | Ok values -> Ok (uniq_sorted (current @ values))
    | Error _ as e -> e
  in
  let remove_list current values =
    let values = uniq_sorted (List.map trim values) in
    Ok (List.filter (fun item -> not (List.exists (String.equal item) values)) current)
  in
  let apply_one user = function
    | Add (User_name, values) | Replace (User_name, values) ->
      one_value "userName" values |> Result.map (fun user_name -> { user with user_name })
    | Remove (User_name, _) -> Error (Invalid_patch "userName cannot be removed")
    | Add (External_id, values) | Replace (External_id, values) ->
      one_value "externalId" values |> Result.map (fun external_id -> { user with external_id })
    | Remove (External_id, _) -> Error (Invalid_patch "externalId cannot be removed")
    | Add (Active, values) | Replace (Active, values) ->
      bool_of_values values |> Result.map (fun active -> { user with active })
    | Remove (Active, _) -> Ok { user with active = false }
    | Add (Display_name, values) | Replace (Display_name, values) ->
      one_value "displayName" values |> Result.map (fun display_name -> { user with display_name = Some display_name })
    | Remove (Display_name, _) -> Ok { user with display_name = None }
    | Add (Emails, values) ->
      add_list user.emails normalize_emails values |> Result.map (fun emails -> { user with emails })
    | Replace (Emails, values) ->
      replace_list normalize_emails values |> Result.map (fun emails -> { user with emails })
    | Remove (Emails, []) -> Ok { user with emails = [] }
    | Remove (Emails, values) -> (
      match normalize_emails values with
      | Error _ as e -> e
      | Ok values -> remove_list user.emails values |> Result.map (fun emails -> { user with emails }))
    | Add (Groups, values) ->
      add_list user.groups (normalize_ref_list (fun s -> Invalid_patch s)) values |> Result.map (fun groups -> { user with groups })
    | Replace (Groups, values) ->
      replace_list (normalize_ref_list (fun s -> Invalid_patch s)) values |> Result.map (fun groups -> { user with groups })
    | Remove (Groups, []) -> Ok { user with groups = [] }
    | Remove (Groups, values) -> (
      match normalize_ref_list (fun s -> Invalid_patch s) values with
      | Error _ as e -> e
      | Ok values -> remove_list user.groups values |> Result.map (fun groups -> { user with groups }))
  in
  List.fold_left (fun acc op -> Result.bind acc (fun user -> apply_one user op)) (Ok user) ops

let apply_group_patch group ops =
  let add_list current values =
    normalize_ref_list (fun s -> Invalid_patch s) values
    |> Result.map (fun values -> uniq_sorted (current @ values))
  in
  let replace_list values = normalize_ref_list (fun s -> Invalid_patch s) values in
  let remove_list current values =
    let values = uniq_sorted (List.map trim values) in
    Ok (List.filter (fun item -> not (List.exists (String.equal item) values)) current)
  in
  let apply_one group = function
    | Group_add (Group_display_name, values) | Group_replace (Group_display_name, values) ->
      one_value "displayName" values |> Result.map (fun display_name -> { group with display_name })
    | Group_remove (Group_display_name, _) -> Error (Invalid_patch "displayName cannot be removed")
    | Group_add (Group_external_id, values) | Group_replace (Group_external_id, values) ->
      one_value "externalId" values |> Result.map (fun external_id -> { group with external_id })
    | Group_remove (Group_external_id, _) -> Error (Invalid_patch "externalId cannot be removed")
    | Group_add (Group_members, values) ->
      add_list group.members values |> Result.map (fun members -> { group with members })
    | Group_replace (Group_members, values) ->
      replace_list values |> Result.map (fun members -> { group with members })
    | Group_remove (Group_members, []) -> Ok { group with members = [] }
    | Group_remove (Group_members, values) -> (
      match normalize_ref_list (fun s -> Invalid_patch s) values with
      | Error _ as e -> e
      | Ok values -> remove_list group.members values |> Result.map (fun members -> { group with members }))
  in
  List.fold_left (fun acc op -> Result.bind acc (fun group -> apply_one group op)) (Ok group) ops

let equal_user (a : user) (b : user) =
  a.external_id = b.external_id
  && a.user_name = b.user_name
  && a.active = b.active
  && a.emails = b.emails
  && a.display_name = b.display_name
  && a.groups = b.groups

let plan_user connection ~existing ~incoming =
  match existing with
  | None -> Ok (Create_user incoming)
  | Some before when equal_user before incoming -> Ok No_user_change
  | Some before when before.external_id <> incoming.external_id ->
    Error (Invalid_user "externalId cannot change for an existing SCIM user")
  | Some before when before.active && not incoming.active && connection.allow_deprovision ->
    Ok (Deprovision_user { before; after = incoming })
  | Some before when before.active && not incoming.active ->
    Ok (Update_user { before; after = { incoming with active = before.active } })
  | Some before -> Ok (Update_user { before; after = incoming })

let equal_group (a : group) (b : group) =
  a.external_id = b.external_id && a.display_name = b.display_name && a.members = b.members

let plan_group ~existing ~incoming =
  match (existing, incoming) with
  | None, None -> Ok No_group_change
  | None, Some incoming -> Ok (Create_group incoming)
  | Some before, None -> Ok (Delete_group before)
  | Some before, Some after when equal_group before after -> Ok No_group_change
  | Some before, Some after when before.external_id <> after.external_id ->
    Error (Invalid_group "externalId cannot change for an existing SCIM group")
  | Some before, Some after -> Ok (Update_group { before; after })

let membership_delta ~before ~after =
  let added = List.filter (fun id -> not (List.exists (String.equal id) before.members)) after.members in
  let removed = List.filter (fun id -> not (List.exists (String.equal id) after.members)) before.members in
  { add = added; remove = removed }

type store = {
  find_connection : string -> connection option;
  list_connections : ?org_id:string -> unit -> connection list;
  upsert_connection : connection -> (unit, string) result;
  delete_connection : string -> (bool, string) result;
  find_user : connection_id:string -> external_id:string -> user option;
  list_users : ?connection_id:string -> unit -> user list;
  upsert_user : connection_id:string -> user -> (unit, string) result;
  delete_user : connection_id:string -> external_id:string -> (bool, string) result;
  find_group : connection_id:string -> external_id:string -> group option;
  list_groups : ?connection_id:string -> unit -> group list;
  upsert_group : connection_id:string -> group -> (unit, string) result;
  delete_group : connection_id:string -> external_id:string -> (bool, string) result;
}

let scoped_key ~connection_id ~external_id = connection_id ^ "\000" ^ external_id

let memory_store () =
  let connections : (string, connection) Hashtbl.t = Hashtbl.create 16 in
  let users : (string, string * user) Hashtbl.t = Hashtbl.create 128 in
  let groups : (string, string * group) Hashtbl.t = Hashtbl.create 64 in
  let mutex = Mutex.create () in
  let locked f = Mutex.lock mutex; Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f in
  let find_connection id = locked (fun () -> Hashtbl.find_opt connections id) in
  let list_connections ?org_id () =
    locked (fun () ->
        Hashtbl.to_seq_values connections
        |> List.of_seq
        |> List.filter (fun c -> Option.fold ~none:true ~some:(String.equal c.org_id) org_id)
        |> List.sort (fun (a : connection) (b : connection) -> String.compare a.id b.id))
  in
  let upsert_connection (connection : connection) =
    locked (fun () ->
        Hashtbl.replace connections connection.id connection;
        Ok ())
  in
  let delete_connection id =
    locked (fun () ->
        let existed = Hashtbl.mem connections id in
        Hashtbl.remove connections id;
        Ok existed)
  in
  let find_user ~connection_id ~external_id =
    locked (fun () -> Option.map snd (Hashtbl.find_opt users (scoped_key ~connection_id ~external_id)))
  in
  let list_users ?connection_id () : user list =
    locked (fun () ->
        let rows : (string * user) list = Hashtbl.to_seq_values users |> List.of_seq in
        rows
        |> List.filter (fun (cid, _) -> Option.fold ~none:true ~some:(String.equal cid) connection_id)
        |> List.map snd
        |> List.sort (fun (a : user) (b : user) -> String.compare a.external_id b.external_id))
  in
  let upsert_user ~connection_id (user : user) =
    locked (fun () ->
        Hashtbl.replace users (scoped_key ~connection_id ~external_id:user.external_id) (connection_id, user);
        Ok ())
  in
  let delete_user ~connection_id ~external_id =
    locked (fun () ->
        let key = scoped_key ~connection_id ~external_id in
        let existed = Hashtbl.mem users key in
        Hashtbl.remove users key;
        Ok existed)
  in
  let find_group ~connection_id ~external_id =
    locked (fun () -> Option.map snd (Hashtbl.find_opt groups (scoped_key ~connection_id ~external_id)))
  in
  let list_groups ?connection_id () : group list =
    locked (fun () ->
        let rows : (string * group) list = Hashtbl.to_seq_values groups |> List.of_seq in
        rows
        |> List.filter (fun (cid, _) -> Option.fold ~none:true ~some:(String.equal cid) connection_id)
        |> List.map snd
        |> List.sort (fun (a : group) (b : group) -> String.compare a.external_id b.external_id))
  in
  let upsert_group ~connection_id (group : group) =
    locked (fun () ->
        Hashtbl.replace groups (scoped_key ~connection_id ~external_id:group.external_id) (connection_id, group);
        Ok ())
  in
  let delete_group ~connection_id ~external_id =
    locked (fun () ->
        let key = scoped_key ~connection_id ~external_id in
        let existed = Hashtbl.mem groups key in
        Hashtbl.remove groups key;
        Ok existed)
  in
  {
    find_connection;
    list_connections;
    upsert_connection;
    delete_connection;
    find_user;
    list_users;
    upsert_user;
    delete_user;
    find_group;
    list_groups;
    upsert_group;
    delete_group;
  }

(* ---- inline tests ---- *)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_connection ?allow_deprovision () =
  ok
    (connection ?allow_deprovision ~id:"Main" ~org_id:"Acme" ~bearer_token:"super-secret-scim-token"
       ())

let test_user ?active ?emails ?display_name ?groups external_id =
  ok (user ?active ?emails ?display_name ?groups ~external_id ~user_name:(external_id ^ "@example.com") ())

let test_group ?members external_id = ok (group ?members ~external_id ~display_name:("Group " ^ external_id) ())

let%test "connection normalizes ids and authenticates hashed bearer tokens" =
  let c = test_connection () in
  c.id = "main"
  && c.org_id = "acme"
  && c.default_role = "member"
  && c.token_hash <> "super-secret-scim-token"
  && authenticate c ~bearer_token:"super-secret-scim-token" = Ok ()
  && authenticate c ~bearer_token:"wrong-token" = Error Invalid_token

let%test "user normalizes emails groups and required external id" =
  let u =
    ok
      (user ~emails:[ "ADA@example.COM"; "ada@example.com" ] ~groups:[ " engineers "; "admins"; "engineers" ]
         ~external_id:" ext-1 " ~user_name:" ada " ())
  in
  u.external_id = "ext-1"
  && u.user_name = "ada"
  && u.emails = [ "ada@example.com" ]
  && u.groups = [ "admins"; "engineers" ]
  && Result.is_error (user ~external_id:"" ~user_name:"ada" ())

let%test "identity scopes external id to org" =
  let c = test_connection () in
  let u = test_user "external-1" in
  match identity c u with
  | Ok key -> Identity.kind key = Identity.Scim && Identity.namespace key = Some "acme" && Identity.subject key = "external-1"
  | Error _ -> false

let%test "membership maps active flag to org membership state" =
  let c = test_connection () in
  let active = test_user "u1" in
  let inactive = test_user ~active:false "u2" in
  let m1 = ok (membership ~now:(fun () -> 10.) c ~user_id:"user-1" active) in
  let m2 = ok (membership ~now:(fun () -> 10.) c ~user_id:"user-2" inactive) in
  Org.is_active_membership m1
  && not (Org.is_active_membership m2)
  && m1.external_id = Some "u1"
  && m1.role = "member"

let%test "apply_user_patch supports scalar and set semantics" =
  let u = test_user ~emails:[ "a@example.com" ] ~groups:[ "dev" ] "u1" in
  match
    apply_user_patch u
      [
        Add (Emails, [ "B@example.com"; "a@example.com" ]);
        Replace (Display_name, [ "Ada" ]);
        Add (Groups, [ "ops"; "dev" ]);
        Remove (Groups, [ "dev" ]);
      ]
  with
  | Ok u -> u.emails = [ "a@example.com"; "b@example.com" ] && u.display_name = Some "Ada" && u.groups = [ "ops" ]
  | Error _ -> false

let%test "apply_user_patch rejects removing external identity" =
  let u = test_user "u1" in
  apply_user_patch u [ Remove (External_id, []) ] = Error (Invalid_patch "externalId cannot be removed")

let%test "apply_group_patch supports member set semantics" =
  let group = test_group ~members:[ "u1"; "u2" ] "g1" in
  match
    apply_group_patch group
      [
        Group_add (Group_members, [ "u2"; "u3" ]);
        Group_replace (Group_display_name, [ "Admins" ]);
        Group_remove (Group_members, [ "u1" ]);
      ]
  with
  | Ok group -> group.display_name = "Admins" && group.members = [ "u2"; "u3" ]
  | Error _ -> false

let%test "apply_group_patch rejects removing external identity" =
  let group = test_group "g1" in
  apply_group_patch group [ Group_remove (Group_external_id, []) ]
  = Error (Invalid_patch "externalId cannot be removed")

let%test "plan_user is idempotent and detects external id changes" =
  let c = test_connection () in
  let before = test_user ~emails:[ "a@example.com" ] "u1" in
  let after = { before with emails = [ "b@example.com" ] } in
  plan_user c ~existing:None ~incoming:before = Ok (Create_user before)
  && plan_user c ~existing:(Some before) ~incoming:before = Ok No_user_change
  && plan_user c ~existing:(Some before) ~incoming:after = Ok (Update_user { before; after })
  && Result.is_error (plan_user c ~existing:(Some before) ~incoming:{ before with external_id = "u2" })

let%test "plan_user deprovisions only when connection allows it" =
  let before = test_user "u1" in
  let after = { before with active = false } in
  let allow = test_connection () in
  let deny = test_connection ~allow_deprovision:false () in
  plan_user allow ~existing:(Some before) ~incoming:after = Ok (Deprovision_user { before; after })
  && plan_user deny ~existing:(Some before) ~incoming:after = Ok (Update_user { before; after = before })

let%test "group plans are idempotent and deletion is explicit" =
  let before = test_group ~members:[ "u1"; "u2" ] "g1" in
  let after = test_group ~members:[ "u2"; "u3" ] "g1" in
  plan_group ~existing:None ~incoming:(Some before) = Ok (Create_group before)
  && plan_group ~existing:(Some before) ~incoming:(Some before) = Ok No_group_change
  && plan_group ~existing:(Some before) ~incoming:(Some after) = Ok (Update_group { before; after })
  && plan_group ~existing:(Some before) ~incoming:None = Ok (Delete_group before)

let%test "membership_delta is deterministic by external id" =
  let before = test_group ~members:[ "u1"; "u2" ] "g1" in
  let after = test_group ~members:[ "u2"; "u3" ] "g1" in
  membership_delta ~before ~after = { add = [ "u3" ]; remove = [ "u1" ] }

let%test "memory_store persists SCIM connection users and groups by tenant connection" =
  let store = memory_store () in
  let connection = test_connection () in
  let user = test_user ~emails:[ "a@example.com" ] "u1" in
  let group = test_group ~members:[ "u1" ] "g1" in
  store.upsert_connection connection = Ok ()
  && store.upsert_user ~connection_id:connection.id user = Ok ()
  && store.upsert_group ~connection_id:connection.id group = Ok ()
  && store.find_connection connection.id = Some connection
  && store.find_user ~connection_id:connection.id ~external_id:"u1" = Some user
  && store.find_group ~connection_id:connection.id ~external_id:"g1" = Some group
  && store.list_users ~connection_id:connection.id () = [ user ]
