type kind =
  | Password
  | Email
  | OAuth
  | Oidc
  | Saml
  | Passkey
  | Scim
  | Recovery

type scope = Global | Per_user
type verification = Verified | Unverified

type key = {
  kind : kind;
  scope : scope;
  namespace : string option;
  subject : string;
  verification : verification option;
}

type link = {
  user_id : string;
  key : key;
  created_at : float;
  verified_at : float option;
}

type link_plan =
  | Attach of link
  | Already_linked of link
  | Conflict of link

type detach_plan =
  | Detach of link
  | Link_not_found
  | Reject_last_credential

type merge_conflict = {
  key : key;
  source : link;
  existing : link;
}

type merge_plan = {
  from_user_id : string;
  into_user_id : string;
  move : link list;
  keep : link list;
  conflicts : merge_conflict list;
}

type error = Blank of string | Invalid_email of string | Invalid_name of string

let string_of_kind = function
  | Password -> "password"
  | Email -> "email"
  | OAuth -> "oauth"
  | Oidc -> "oidc"
  | Saml -> "saml"
  | Passkey -> "passkey"
  | Scim -> "scim"
  | Recovery -> "recovery"

let kind_of_string = function
  | "password" -> Some Password
  | "email" -> Some Email
  | "oauth" -> Some OAuth
  | "oidc" -> Some Oidc
  | "saml" -> Some Saml
  | "passkey" -> Some Passkey
  | "scim" -> Some Scim
  | "recovery" -> Some Recovery
  | _ -> None

let string_of_scope = function Global -> "global" | Per_user -> "per_user"
let string_of_verification = function Verified -> "verified" | Unverified -> "unverified"

let string_of_error = function
  | Blank name -> name ^ " cannot be blank"
  | Invalid_email email -> "Invalid email identity: " ^ email
  | Invalid_name name -> "Invalid identity name: " ^ name

let trim = String.trim
let lower_trim s = String.lowercase_ascii (trim s)
let nonblank s = trim s <> ""
let blank name s = if nonblank s then Ok (trim s) else Error (Blank name)
let blank_lower name s = if nonblank s then Ok (lower_trim s) else Error (Blank name)

let key ~kind ~scope ?namespace ?verification subject =
  { kind; scope; namespace; subject; verification }

let password () = key ~kind:Password ~scope:Per_user "password"

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

let email ~verified raw =
  let email = lower_trim raw in
  if email = "" then Error (Blank "email")
  else if not (valid_email email) then Error (Invalid_email raw)
  else
    let verification = if verified then Verified else Unverified in
    Ok (key ~kind:Email ~scope:Global ~verification email)

let bind r f = match r with Ok x -> f x | Error _ as e -> e

let oauth ~provider ~subject =
  bind (blank_lower "provider" provider) (fun provider ->
      bind (blank "subject" subject) (fun subject ->
          Ok (key ~kind:OAuth ~scope:Global ~namespace:provider subject)))

let oidc ~issuer ~connection ~subject =
  bind (blank "issuer" issuer) (fun issuer ->
      bind (blank_lower "connection" connection) (fun connection ->
          bind (blank "subject" subject) (fun subject ->
              Ok (key ~kind:Oidc ~scope:Global ~namespace:(issuer ^ "\000" ^ connection) subject))))

let saml ~connection ~name_id ?external_id () =
  bind (blank_lower "connection" connection) (fun connection ->
      bind (blank "name_id" name_id) (fun name_id ->
          match external_id with
          | Some external_id when nonblank external_id ->
            Ok (key ~kind:Saml ~scope:Global ~namespace:connection (trim external_id))
          | Some _ -> Error (Blank "external_id")
          | None -> Ok (key ~kind:Saml ~scope:Global ~namespace:connection name_id)))

let passkey ~credential_id ?user_handle () =
  bind (blank "credential_id" credential_id) (fun credential_id ->
      match user_handle with
      | Some user_handle when not (nonblank user_handle) -> Error (Blank "user_handle")
      | _ -> Ok (key ~kind:Passkey ~scope:Global credential_id))

let scim ~org_id ~external_id =
  bind (blank_lower "org_id" org_id) (fun org_id ->
      bind (blank "external_id" external_id) (fun external_id ->
          Ok (key ~kind:Scim ~scope:Global ~namespace:org_id external_id)))

let recovery ~name =
  bind (blank_lower "name" name) (fun name ->
      let valid =
        String.for_all
          (function
            | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' -> true
            | _ -> false)
          name
      in
      if valid then Ok (key ~kind:Recovery ~scope:Per_user name) else Error (Invalid_name name))

let kind k = k.kind
let scope k = k.scope
let namespace k = k.namespace
let subject k = k.subject
let verification k = k.verification

let frame s = string_of_int (String.length s) ^ ":" ^ s

let stable_key k =
  String.concat ""
    [
      frame "fennec-identity-v1";
      frame (string_of_scope k.scope);
      frame (string_of_kind k.kind);
      frame (Option.value ~default:"" k.namespace);
      frame k.subject;
    ]

let describe k =
  match (k.kind, k.namespace, k.verification) with
  | Password, _, _ -> "password"
  | Recovery, _, _ -> "recovery:" ^ k.subject
  | Email, _, Some v -> "email:" ^ k.subject ^ ":" ^ string_of_verification v
  | OAuth, Some provider, _ -> "oauth:" ^ provider ^ ":" ^ k.subject
  | Oidc, Some ns, _ -> "oidc:" ^ String.map (function '\000' -> '/' | c -> c) ns ^ ":" ^ k.subject
  | Saml, Some connection, _ -> "saml:" ^ connection ^ ":" ^ k.subject
  | Passkey, Some user_handle, _ -> "passkey:" ^ user_handle ^ ":" ^ k.subject
  | Passkey, None, _ -> "passkey:" ^ k.subject
  | Scim, Some org_id, _ -> "scim:" ^ org_id ^ ":" ^ k.subject
  | _, _, _ -> string_of_kind k.kind ^ ":" ^ k.subject

let equal a b = stable_key a = stable_key b
let compare a b = String.compare (stable_key a) (stable_key b)

let is_verified_email k = match (k.kind, k.verification) with Email, Some Verified -> true | _ -> false

let same_verified_email a b =
  is_verified_email a && is_verified_email b && String.equal a.subject b.subject

let link ?verified_at ~user_id key ~created_at = { user_id; key; created_at; verified_at }

let plan_link ?verified_at ~created_at ~user_id key ~existing =
  let next = link ?verified_at ~user_id key ~created_at in
  match existing with
  | None -> Attach next
  | Some existing when existing.user_id = user_id && equal existing.key key -> Already_linked existing
  | Some _ when key.scope = Per_user -> Attach next
  | Some existing -> Conflict existing

let usable_for_login k =
  match (k.kind, k.verification) with
  | Email, Some Unverified -> false
  | Password, _ | Email, Some Verified | OAuth, _ | Oidc, _ | Saml, _ | Passkey, _ | Scim, _ | Recovery, _ ->
    true
  | Email, None -> false

let plan_detach ?(allow_last = false) ~user_id key ~links =
  let target =
    List.find_opt (fun link -> link.user_id = user_id && equal link.key key) links
  in
  match target with
  | None -> Link_not_found
  | Some target ->
    if allow_last || not (usable_for_login target.key) then Detach target
    else
      let remaining_usable =
        List.exists
          (fun link ->
            link.user_id = user_id && not (equal link.key key) && usable_for_login link.key)
          links
      in
      if remaining_usable then Detach target else Reject_last_credential

let sort_links (links : link list) =
  List.sort
    (fun (a : link) (b : link) ->
      match compare a.key b.key with
      | 0 -> String.compare a.user_id b.user_id
      | n -> n)
    links

let plan_merge ~from_user_id ~into_user_id ~(source : link list) ~(target : link list) =
  let target_by_key =
    let table = Hashtbl.create (List.length target) in
    List.iter
      (fun (link : link) ->
        if link.key.scope = Global then Hashtbl.replace table (stable_key link.key) link)
      target;
    table
  in
  let target_local key =
    List.find_opt
      (fun (link : link) -> link.user_id = into_user_id && equal link.key key)
      target
  in
  let move : link list ref = ref [] in
  let keep : link list ref = ref [] in
  let conflicts : merge_conflict list ref = ref [] in
  let source = List.filter (fun link -> link.user_id = from_user_id) source |> sort_links in
  List.iter
    (fun (source_link : link) ->
      if source_link.key.scope = Per_user then
        match target_local source_link.key with
        | Some existing -> keep := existing :: !keep
        | None -> move := { source_link with user_id = into_user_id } :: !move
      else
        match Hashtbl.find_opt target_by_key (stable_key source_link.key) with
        | None -> move := { source_link with user_id = into_user_id } :: !move
        | Some existing when existing.user_id = into_user_id -> keep := existing :: !keep
        | Some existing ->
          conflicts := { key = source_link.key; source = source_link; existing } :: !conflicts)
    source;
  {
    from_user_id;
    into_user_id;
    move = sort_links !move;
    keep = sort_links !keep;
    conflicts = List.sort (fun (a : merge_conflict) (b : merge_conflict) -> compare a.key b.key) !conflicts;
  }

type store = {
  find : key -> link option;
  list : ?user_id:string -> unit -> link list;
  attach : ?verified_at:float -> created_at:float -> user_id:string -> key -> link_plan;
  detach : ?allow_last:bool -> user_id:string -> key -> detach_plan;
  merge : from_user_id:string -> into_user_id:string -> (merge_plan, merge_conflict list) result;
}

let memory_store () =
  let links : link list ref = ref [] in
  let mutex = Mutex.create () in
  let locked f =
    Mutex.lock mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f
  in
  let find_unlocked key =
    if key.scope = Per_user then None
    else List.find_opt (fun (link : link) -> link.key.scope = Global && equal link.key key) !links
  in
  let list ?user_id () =
    locked (fun () ->
        let links =
          match user_id with
          | None -> !links
          | Some user_id -> List.filter (fun (link : link) -> link.user_id = user_id) !links
        in
        sort_links links)
  in
  let find key = locked (fun () -> find_unlocked key) in
  let attach ?verified_at ~created_at ~user_id key =
    locked (fun () ->
        let exact =
          List.find_opt (fun (link : link) -> link.user_id = user_id && equal link.key key) !links
        in
        let existing = match exact with Some _ as found -> found | None -> find_unlocked key in
        match plan_link ?verified_at ~created_at ~user_id key ~existing with
        | Attach link as plan ->
          links := link :: !links;
          plan
        | Already_linked _ as plan -> plan
        | Conflict _ as plan -> plan)
  in
  let detach ?allow_last ~user_id key =
    locked (fun () ->
        match plan_detach ?allow_last ~user_id key ~links:!links with
        | Detach link as plan ->
          links :=
            List.filter
              (fun (existing : link) -> not (existing.user_id = link.user_id && equal existing.key link.key))
              !links;
          plan
        | Link_not_found as plan -> plan
        | Reject_last_credential as plan -> plan)
  in
  let merge ~from_user_id ~into_user_id =
    locked (fun () ->
        let source = List.filter (fun (link : link) -> link.user_id = from_user_id) !links in
        let target = List.filter (fun (link : link) -> link.user_id <> from_user_id) !links in
        let plan = plan_merge ~from_user_id ~into_user_id ~source ~target in
        match plan.conflicts with
        | _ :: _ as conflicts -> Error conflicts
        | [] ->
          let moved_keys = List.map (fun (link : link) -> stable_key link.key) source in
          let moved key = List.exists (String.equal (stable_key key)) moved_keys in
          links :=
            List.filter (fun (link : link) -> not (link.user_id = from_user_id && moved link.key)) !links
            @ plan.move;
          Ok plan)
  in
  { find; list; attach; detach; merge }

(* ---- inline tests ---- *)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let%test "kind names round-trip" =
  List.for_all
    (fun k -> kind_of_string (string_of_kind k) = Some k)
    [ Password; Email; OAuth; Oidc; Saml; Passkey; Scim; Recovery ]

let%test "email normalizes case and verification state" =
  let k = ok (email ~verified:true " ADA@example.COM ") in
  kind k = Email
  && scope k = Global
  && subject k = "ada@example.com"
  && verification k = Some Verified
  && is_verified_email k

let%test "invalid emails are rejected" =
  List.for_all
    (fun raw -> Result.is_error (email ~verified:true raw))
    [ ""; "ada"; "@example.com"; "ada@"; "ada @example.com"; "ada@example .com" ]

let%test "unverified email never satisfies verified auto-link evidence" =
  let a = ok (email ~verified:true "ada@example.com") in
  let b = ok (email ~verified:false "ADA@example.com") in
  equal a b && not (is_verified_email b) && not (same_verified_email a b)

let%test "same verified email auto-link evidence ignores case" =
  let a = ok (email ~verified:true "ada@example.com") in
  let b = ok (email ~verified:true "ADA@example.COM") in
  same_verified_email a b && equal a b

let%test "provider and connection names normalize, subjects do not" =
  let oauth_key = ok (oauth ~provider:" GitHub " ~subject:" Subject ") in
  let oidc_key = ok (oidc ~issuer:" https://idp.example/ " ~connection:" Main " ~subject:" Sub ") in
  namespace oauth_key = Some "github"
  && subject oauth_key = "Subject"
  && namespace oidc_key = Some "https://idp.example/\000main"
  && subject oidc_key = "Sub"

let%test "oidc issuer participates in uniqueness" =
  let a = ok (oidc ~issuer:"https://idp-a.example" ~connection:"main" ~subject:"sub") in
  let b = ok (oidc ~issuer:"https://idp-b.example" ~connection:"main" ~subject:"sub") in
  not (equal a b)

let%test "saml prefers external_id when present" =
  let k = ok (saml ~connection:" Corp " ~name_id:"display@example.com" ~external_id:" stable-id " ()) in
  kind k = Saml && namespace k = Some "corp" && subject k = "stable-id"

let%test "passkey credential is global and ignores user handle for uniqueness" =
  let k = ok (passkey ~credential_id:" credential-id " ~user_handle:" handle " ()) in
  let same = ok (passkey ~credential_id:" credential-id " ~user_handle:" other-handle " ()) in
  kind k = Passkey && scope k = Global && namespace k = None && subject k = "credential-id" && equal k same

let%test "scim org id scopes external ids" =
  let a = ok (scim ~org_id:" OrgA " ~external_id:"123") in
  let b = ok (scim ~org_id:" OrgB " ~external_id:"123") in
  not (equal a b) && namespace a = Some "orga" && namespace b = Some "orgb"

let%test "password and recovery are per-user identities" =
  let p = password () in
  let r = ok (recovery ~name:"backup-code") in
  scope p = Per_user && scope r = Per_user

let%test "recovery names are conservative ascii identifiers" =
  Result.is_ok (recovery ~name:"totp.backup-code_1") && Result.is_error (recovery ~name:"backup code")

let%test "stable keys are separator-collision safe" =
  let a = ok (oauth ~provider:"a:b" ~subject:"c") in
  let b = ok (oauth ~provider:"a" ~subject:"b:c") in
  stable_key a <> stable_key b

let%test "compare follows stable key" =
  let items =
    [
      ok (oauth ~provider:"github" ~subject:"2");
      ok (email ~verified:true "a@example.com");
      ok (oauth ~provider:"github" ~subject:"1");
    ]
  in
  let sorted = List.sort compare items in
  sorted = List.sort (fun a b -> String.compare (stable_key a) (stable_key b)) items

let%test "plan_link attaches idempotently and rejects global collisions" =
  let key = ok (oauth ~provider:"github" ~subject:"ada") in
  let existing = link ~user_id:"u1" key ~created_at:1. in
  plan_link ~created_at:2. ~user_id:"u1" key ~existing:(Some existing) = Already_linked existing
  && plan_link ~created_at:2. ~user_id:"u2" key ~existing:(Some existing) = Conflict existing
  &&
  match plan_link ~created_at:2. ~user_id:"u1" key ~existing:None with
  | Attach link -> link.user_id = "u1" && equal link.key key
  | _ -> false

let%test "plan_link allows per-user credential facts for different users" =
  let key = password () in
  let existing = link ~user_id:"u1" key ~created_at:1. in
  match plan_link ~created_at:2. ~user_id:"u2" key ~existing:(Some existing) with
  | Attach next -> next.user_id = "u2" && equal next.key key
  | _ -> false

let%test "usable_for_login rejects unverified email only" =
  usable_for_login (ok (email ~verified:true "ada@example.com"))
  && not (usable_for_login (ok (email ~verified:false "ada@example.com")))
  && usable_for_login (password ())
  && usable_for_login (ok (passkey ~credential_id:"cred" ()))

let%test "plan_detach rejects the last usable credential by default" =
  let email_key = ok (email ~verified:true "ada@example.com") in
  let unverified = ok (email ~verified:false "ada@example.com") in
  let password_key = password () in
  let email_link = link ~user_id:"u1" email_key ~created_at:1. in
  let password_link = link ~user_id:"u1" password_key ~created_at:1. in
  let unverified_link = link ~user_id:"u1" unverified ~created_at:1. in
  plan_detach ~user_id:"u1" email_key ~links:[ email_link ] = Reject_last_credential
  && plan_detach ~user_id:"u1" email_key ~links:[ email_link; password_link ] = Detach email_link
  && plan_detach ~user_id:"u1" unverified ~links:[ unverified_link ] = Detach unverified_link
  && plan_detach ~allow_last:true ~user_id:"u1" email_key ~links:[ email_link ] = Detach email_link

let%test "plan_merge moves missing links and keeps target duplicates" =
  let github = ok (oauth ~provider:"github" ~subject:"ada") in
  let email_key = ok (email ~verified:true "ada@example.com") in
  let passkey_key = ok (passkey ~credential_id:"cred" ()) in
  let source =
    [
      link ~user_id:"old" github ~created_at:1.;
      link ~user_id:"old" email_key ~created_at:1.;
      link ~user_id:"ignored" passkey_key ~created_at:1.;
    ]
  in
  let target = [ link ~user_id:"new" email_key ~created_at:2. ] in
  let plan = plan_merge ~from_user_id:"old" ~into_user_id:"new" ~source ~target in
  plan.from_user_id = "old"
  && plan.into_user_id = "new"
  && List.map (fun link -> (link.user_id, describe link.key)) plan.move = [ ("new", "oauth:github:ada") ]
  && List.map (fun link -> (link.user_id, describe link.key)) plan.keep = [ ("new", "email:ada@example.com:verified") ]
  && plan.conflicts = []

let%test "plan_merge reports third-user conflicts" =
  let github = ok (oauth ~provider:"github" ~subject:"ada") in
  let source = [ link ~user_id:"old" github ~created_at:1. ] in
  let target = [ link ~user_id:"other" github ~created_at:2. ] in
  match (plan_merge ~from_user_id:"old" ~into_user_id:"new" ~source ~target).conflicts with
  | [ conflict ] ->
    conflict.source.user_id = "old" && conflict.existing.user_id = "other" && equal conflict.key github
  | _ -> false

let%test "memory_store attach enforces global uniqueness" =
  let store = memory_store () in
  let github = ok (oauth ~provider:"github" ~subject:"ada") in
  match store.attach ~created_at:1. ~user_id:"u1" github with
  | Attach first ->
    store.find github = Some first
    && store.attach ~created_at:2. ~user_id:"u1" github = Already_linked first
    && store.attach ~created_at:2. ~user_id:"u2" github = Conflict first
  | _ -> false

let%test "memory_store stores per-user credential facts independently" =
  let store = memory_store () in
  let password = password () in
  match (store.attach ~created_at:1. ~user_id:"u1" password, store.attach ~created_at:1. ~user_id:"u2" password) with
  | Attach a, Attach b ->
    store.find password = None
    && List.map (fun link -> link.user_id) (store.list ()) = [ "u1"; "u2" ]
    && equal a.key b.key
  | _ -> false

let%test "memory_store detach mutates only when plan allows it" =
  let store = memory_store () in
  let email_key = ok (email ~verified:true "ada@example.com") in
  let password_key = password () in
  ignore (store.attach ~created_at:1. ~user_id:"u1" email_key);
  store.detach ~user_id:"u1" email_key = Reject_last_credential
  && store.find email_key <> None
  &&
  match store.attach ~created_at:1. ~user_id:"u1" password_key with
  | Attach _ ->
    (match store.detach ~user_id:"u1" email_key with Detach _ -> store.find email_key = None | _ -> false)
  | _ -> false

let%test "memory_store merge moves source links atomically" =
  let store = memory_store () in
  let github = ok (oauth ~provider:"github" ~subject:"ada") in
  let email_key = ok (email ~verified:true "ada@example.com") in
  let password_key = password () in
  ignore (store.attach ~created_at:1. ~user_id:"old" github);
  ignore (store.attach ~created_at:1. ~user_id:"old" password_key);
  ignore (store.attach ~created_at:1. ~user_id:"new" email_key);
  match store.merge ~from_user_id:"old" ~into_user_id:"new" with
  | Error _ -> false
  | Ok plan ->
    List.length plan.move = 2
    && store.list ~user_id:"old" () = []
    && List.length (store.list ~user_id:"new" ()) = 3
    &&
    match store.find github with
    | Some link -> link.user_id = "new"
    | None -> false

let%test "memory_store prevents conflicting global source state through attach" =
  let store = memory_store () in
  let github = ok (oauth ~provider:"github" ~subject:"ada") in
  let passkey_key = ok (passkey ~credential_id:"cred" ()) in
  ignore (store.attach ~created_at:1. ~user_id:"old" passkey_key);
  ignore (store.attach ~created_at:1. ~user_id:"other" github);
  store.attach ~created_at:1. ~user_id:"old" github = Conflict (Option.get (store.find github))
  && store.list ~user_id:"old" () = [ link ~user_id:"old" passkey_key ~created_at:1. ]
