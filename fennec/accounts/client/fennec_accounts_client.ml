module B = Bson

type email = {
  address : string;
  verified : bool;
}

type user = {
  id : string;
  username : string option;
  emails : email list;
  roles : string list;
  profile : Bson.t option;
  status : string option;
  created_at : float option;
  updated_at : float option;
}

type session = {
  user_id : string option;
  user : user option;
}

type selector =
  | By_id of string
  | By_email of string
  | By_username of string

type login_result =
  | Logged_in of {
      id : string;
      token : string;
      user : user option;
    }
  | Mfa_required of {
      user_id : string;
      mfa_token : string;
    }

type error = {
  code : string;
  reason : string;
}

type t = {
  ddp : Ddp_client.t;
  token_key : string option;
  user_sig : user option Fur.signal;
  user_id_sig : string option Fur.signal;
  logging_sig : bool Fur.signal;
  last_error_sig : error option Fur.signal;
  in_flight : int Fur.signal;
}

let default_token_key = "fennec.accounts.loginToken"

let ddp t = t.ddp
let user t = t.user_sig
let user_id t = t.user_id_sig
let logging_in t = t.logging_sig
let last_error t = t.last_error_sig

let error code reason = { code; reason }
let decode_error reason = Error (error "client-decode" reason)

let start_call t =
  Fur.update t.in_flight (fun n -> n + 1);
  Fur.set t.logging_sig true

let finish_call t =
  Fur.update t.in_flight (fun n -> max 0 (n - 1));
  Fur.set t.logging_sig (Fur.peek t.in_flight > 0)

let store_token t token = Option.iter (fun key -> Fur.Browser.local_set key token) t.token_key
let clear_token t = Option.iter Fur.Browser.local_remove t.token_key
let load_token t = Option.bind t.token_key Fur.Browser.local_get

let string_field d name =
  match B.get d name with
  | Some (B.String s) -> Ok s
  | Some B.Null -> Error ("missing field " ^ name)
  | Some _ -> Error (name ^ ": expected string")
  | None -> Error ("missing field " ^ name)

let opt_string_field d name =
  match B.get d name with
  | None | Some B.Null -> Ok None
  | Some (B.String s) -> Ok (Some s)
  | Some _ -> Error (name ^ ": expected string or null")

let opt_float_field d name =
  match B.get d name with
  | None | Some B.Null -> Ok None
  | Some v -> (
    match B.as_float v with Some f -> Ok (Some f) | None -> Error (name ^ ": expected number or null"))

let doc_field d name =
  match B.get d name with
  | Some (B.Document _ as doc) -> Ok (Some doc)
  | None | Some B.Null -> Ok None
  | Some _ -> Error (name ^ ": expected document or null")

let email_of_bson = function
  | B.Document _ as d -> (
    match (B.get_string d "address", B.get_bool d "verified") with
    | Some address, Some verified -> Ok { address; verified }
    | _ -> Error "email expects {address, verified}")
  | _ -> Error "email expects a document"

let rec decode_emails acc = function
  | [] -> Ok (List.rev acc)
  | x :: xs -> (
    match email_of_bson x with Ok e -> decode_emails (e :: acc) xs | Error e -> Error e)

let rec decode_strings field acc = function
  | [] -> Ok (List.rev acc)
  | B.String s :: xs -> decode_strings field (s :: acc) xs
  | _ :: _ -> Error (field ^ ": expected string array")

let decode_user d =
  match d with
  | B.Document _ ->
    let ( let* ) = Result.bind in
    let* id =
      match (B.get_string d "id", B.get_string d "_id") with
      | Some id, _ | None, Some id -> Ok id
      | None, None -> Error "user missing id"
    in
    let* username = opt_string_field d "username" in
    let* emails = match B.get_list d "emails" with Some xs -> decode_emails [] xs | None -> Ok [] in
    let* roles = match B.get_list d "roles" with Some xs -> decode_strings "roles" [] xs | None -> Ok [] in
    let* profile = doc_field d "profile" in
    let* status = opt_string_field d "status" in
    let* created_at = opt_float_field d "createdAt" in
    let* updated_at = opt_float_field d "updatedAt" in
    Ok { id; username; emails; roles; profile; status; created_at; updated_at }
  | _ -> Error "user expects a document"

let decode_optional_user = function
  | None | Some B.Null -> Ok None
  | Some doc -> Result.map Option.some (decode_user doc)

let decode_session d =
  match d with
  | B.Document _ ->
    Result.bind (opt_string_field d "userId") (fun user_id ->
        Result.map (fun user -> { user_id; user }) (decode_optional_user (B.get d "user")))
  | _ -> Error "session expects a document"

let decode_login_result d =
  match d with
  | B.Document _ when B.get_bool d "mfaRequired" = Some true ->
    Result.bind (string_field d "userId") (fun user_id ->
        Result.map (fun mfa_token -> Mfa_required { user_id; mfa_token }) (string_field d "mfaToken"))
  | B.Document _ ->
    Result.bind (string_field d "id") (fun id ->
        Result.bind (string_field d "token") (fun token ->
            Result.map (fun user -> Logged_in { id; token; user }) (decode_optional_user (B.get d "user"))))
  | _ -> Error "login result expects a document"

let selector_bson = function
  | By_id id -> B.doc [ ("id", B.str id) ]
  | By_email email -> B.doc [ ("email", B.str email) ]
  | By_username username -> B.doc [ ("username", B.str username) ]

let apply_session t (session : session) =
  Fur.set t.user_sig session.user;
  Fur.set t.user_id_sig session.user_id

let observe t raw decode apply =
  start_call t;
  let out = Fur.signal None in
  let stop = ref (fun () -> ()) in
  stop :=
    Fur.watch (fun () ->
        match Fur.get raw with
        | None -> ()
        | Some result ->
          !stop ();
          finish_call t;
          let decoded =
            match result with
            | Error (code, reason) -> Error { code; reason }
            | Ok value -> (
              match decode value with
              | Error reason -> decode_error reason
              | Ok value -> apply value)
          in
          (match decoded with Ok _ -> Fur.set t.last_error_sig None | Error e -> Fur.set t.last_error_sig (Some e));
          Fur.set out (Some decoded));
  out

let refresh_user t =
  let raw = Ddp_client.call_result t.ddp ~name:"currentUser" () in
  observe t raw decode_session (fun session ->
      apply_session t session;
      Ok session)

let apply_login_result t = function
  | Mfa_required _ as pending -> Ok pending
  | Logged_in { id; token; user } as logged_in ->
    store_token t token;
    Fur.set t.user_id_sig (Some id);
    (match user with
    | Some user -> Fur.set t.user_sig (Some user)
    | None -> ignore (refresh_user t));
    Ok logged_in

let login_with_password t selector ~password =
  let raw = Ddp_client.call_result t.ddp ~name:"login" ~params:[ selector_bson selector; B.str password ] () in
  observe t raw decode_login_result (apply_login_result t)

let login_with_token t token =
  let raw = Ddp_client.call_result t.ddp ~name:"login" ~params:[ B.doc [ ("resume", B.str token) ] ] () in
  observe t raw decode_login_result (apply_login_result t)

let create_user t ?username ?email ?profile ~password () =
  let fields =
    [ Some ("password", B.str password) ]
    @ [ Option.map (fun username -> ("username", B.str username)) username ]
    @ [ Option.map (fun email -> ("email", B.str email)) email ]
    @ [ Option.map (fun profile -> ("profile", profile)) profile ]
  in
  let raw = Ddp_client.call_result t.ddp ~name:"createUser" ~params:[ B.doc (List.filter_map Fun.id fields) ] () in
  observe t raw decode_login_result (apply_login_result t)

let unit_value = function B.Bool true -> Ok () | B.Bool false -> Ok () | B.Null -> Ok () | _ -> Error "expected unit result"

let logout t =
  clear_token t;
  let raw = Ddp_client.call_result t.ddp ~name:"logout" () in
  observe t raw unit_value (fun () ->
      Fur.set t.user_id_sig None;
      Fur.set t.user_sig None;
      Ok ())

let logout_other_clients t =
  let raw = Ddp_client.call_result t.ddp ~name:"logoutOtherClients" () in
  observe t raw decode_login_result (apply_login_result t)

let change_password t ~old_password ~new_password =
  let raw = Ddp_client.call_result t.ddp ~name:"changePassword" ~params:[ B.str old_password; B.str new_password ] () in
  observe t raw unit_value (fun () -> Ok ())

let reset_password t ~token ~password =
  let raw = Ddp_client.call_result t.ddp ~name:"resetPassword" ~params:[ B.str token; B.str password ] () in
  observe t raw decode_login_result (apply_login_result t)

let enroll_account t ~token ~password =
  let raw = Ddp_client.call_result t.ddp ~name:"enrollAccount" ~params:[ B.str token; B.str password ] () in
  observe t raw decode_login_result (apply_login_result t)

let verify_email t ~token =
  let raw = Ddp_client.call_result t.ddp ~name:"verifyEmail" ~params:[ B.str token ] () in
  observe t raw decode_login_result (apply_login_result t)

let complete_login_step_up_totp t ~mfa_token ~totp_id ~code =
  let params =
    [ B.doc [ ("mfaToken", B.str mfa_token); ("totpId", B.str totp_id); ("code", B.str code) ] ]
  in
  let raw = Ddp_client.call_result t.ddp ~name:"completeLoginStepUp" ~params () in
  observe t raw decode_login_result (apply_login_result t)

let complete_login_step_up_backup t ~mfa_token ~user_id ~code =
  let params =
    [ B.doc [ ("mfaToken", B.str mfa_token); ("userId", B.str user_id); ("backupCode", B.str code) ] ]
  in
  let raw = Ddp_client.call_result t.ddp ~name:"completeLoginStepUp" ~params () in
  observe t raw decode_login_result (apply_login_result t)

let of_ddp ?(token_key = Some default_token_key) ddp =
  let t =
    {
      ddp;
      token_key;
      user_sig = Fur.signal None;
      user_id_sig = Fur.signal None;
      logging_sig = Fur.signal false;
      last_error_sig = Fur.signal None;
      in_flight = Fur.signal 0;
    }
  in
  ignore (refresh_user t);
  (match load_token t with Some token when String.trim token <> "" -> ignore (login_with_token t token) | _ -> ());
  t

let connect ?path ?persist ?chrome ?token_key () =
  of_ddp ?token_key (Ddp_client.connect ?path ?persist ?chrome ())

let default_client : t option ref = ref None

let set_default t = default_client := Some t

let default () =
  match !default_client with
  | Some t -> t
  | None ->
    let t = connect () in
    default_client := Some t;
    t

let current_user_id () = user_id (default ())
let current_user () = user (default ())
let current_logging_in () = logging_in (default ())

let%test "decode_login_result distinguishes MFA from completed login" =
  let logged = B.doc [ ("id", B.str "u1"); ("token", B.str "tok") ] in
  let mfa = B.doc [ ("mfaRequired", B.bool true); ("userId", B.str "u1"); ("mfaToken", B.str "mfa") ] in
  decode_login_result logged = Ok (Logged_in { id = "u1"; token = "tok"; user = None })
  && decode_login_result mfa = Ok (Mfa_required { user_id = "u1"; mfa_token = "mfa" })

let%test "decode_session accepts canonical session payload" =
  let doc =
    B.doc
      [
        ("userId", B.str "u1");
        ( "user",
          B.doc
            [
              ("_id", B.str "u1");
              ("username", B.str "ada");
              ("emails", B.array [ B.doc [ ("address", B.str "ada@example.com"); ("verified", B.bool true) ] ]);
              ("roles", B.array [ B.str "admin" ]);
            ] );
      ]
  in
  match decode_session doc with
  | Ok
      {
        user_id = Some "u1";
        user =
          Some
            {
              id = "u1";
              username = Some "ada";
              emails = [ { address = "ada@example.com"; verified = true } ];
              roles = [ "admin" ];
              _;
            };
      } ->
    true
  | _ -> false
