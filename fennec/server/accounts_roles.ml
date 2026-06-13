type error =
  | Blank of string
  | Invalid_name of string

let string_of_error = function
  | Blank what -> what ^ " cannot be blank"
  | Invalid_name name -> "Invalid role or permission name: " ^ name

let valid_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':' -> true
  | _ -> false

let normalize kind raw =
  let name = String.lowercase_ascii (String.trim raw) in
  if name = "" then Error (Blank kind)
  else if String.for_all valid_char name then Ok name
  else Error (Invalid_name raw)

module Role = struct
  type t = string

  let v s = normalize "role" s
  let v_exn s = match v s with Ok role -> role | Error e -> invalid_arg (string_of_error e)
  let name t = t
  let admin = v_exn "admin"
  let owner = v_exn "owner"
  let member = v_exn "member"
end

module Permission = struct
  type t = string

  let v s = normalize "permission" s
  let v_exn s = match v s with Ok permission -> permission | Error e -> invalid_arg (string_of_error e)
  let name t = t
end

type definition = {
  role : Role.t;
  permissions : Permission.t list;
}

let compare_string = String.compare

let sort_uniq xs =
  let rec dedupe acc = function
    | a :: (b :: _ as rest) when String.equal a b -> dedupe acc rest
    | x :: rest -> dedupe (x :: acc) rest
    | [] -> List.rev acc
  in
  xs |> List.sort compare_string |> dedupe []

let role role permissions = { role; permissions = sort_uniq permissions }

type policy = (Role.t * Permission.t list) list

let policy definitions =
  let table = Hashtbl.create (List.length definitions) in
  List.iter (fun definition -> Hashtbl.replace table definition.role definition.permissions) definitions;
  Hashtbl.to_seq table |> List.of_seq |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let empty_policy = policy []
let role_allows policy ~role ~permission =
  match List.assoc_opt role policy with Some ps -> List.exists (String.equal permission) ps | None -> false
let any_role_allows policy ~roles ~permission = List.exists (fun role -> role_allows policy ~role ~permission) roles

let normalize_roles roles =
  let rec loop acc = function
    | [] -> Ok (sort_uniq acc)
    | role :: rest -> Result.bind (Role.v role) (fun role -> loop (role :: acc) rest)
  in
  loop [] roles

let role_names roles = sort_uniq (List.map Role.name roles)
let mem role roles = List.exists (String.equal role) roles
let add role roles = if mem role roles then roles else sort_uniq (role :: roles)
let remove role roles = List.filter (fun r -> not (String.equal r role)) roles

let%test "roles and permissions normalize to canonical lowercase names" =
  Role.v " Admin " = Ok Role.admin && Result.map Permission.name (Permission.v "Billing.Read") = Ok "billing.read"

let%test "roles reject blank and unsafe names" =
  Result.is_error (Role.v " ") && Result.is_error (Role.v "admin role")

let%test "policy denies by default and grants declared permissions" =
  let admin = Role.admin in
  let read = Permission.v_exn "billing.read" in
  let write = Permission.v_exn "billing.write" in
  let p = policy [ role admin [ read ] ] in
  role_allows p ~role:admin ~permission:read && not (role_allows p ~role:Role.member ~permission:write)

let%test "role set helpers deduplicate canonical roles" =
  match normalize_roles [ "Admin"; "admin"; "support" ] with
  | Ok roles ->
    role_names roles = [ "admin"; "support" ]
    && role_names (remove Role.admin roles) = [ "support" ]
    && role_names (add Role.member roles) = [ "admin"; "member"; "support" ]
  | Error _ -> false
