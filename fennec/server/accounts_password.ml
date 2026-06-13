type hasher = {
  hash : password:string -> string;
  verify : password:string -> hash:string -> bool;
}

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg ->
    failwith ("Fennec.Accounts.Password: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64e s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d s = match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))

let int32_be n =
  String.init 4 (fun i -> Char.chr ((n lsr ((3 - i) * 8)) land 0xff))

let xor_into acc block =
  for i = 0 to Bytes.length acc - 1 do
    Bytes.set acc i (Char.chr (Char.code (Bytes.get acc i) lxor Char.code block.[i]))
  done

let pbkdf2_sha256 ~password ~salt ~iterations =
  if iterations <= 0 then invalid_arg "Fennec.Accounts.Password.password_hasher: iterations must be positive";
  let u = ref (hmac_sha256 ~key:password (salt ^ int32_be 1)) in
  let acc = Bytes.of_string !u in
  for _ = 2 to iterations do
    u := hmac_sha256 ~key:password !u;
    xor_into acc !u
  done;
  Bytes.unsafe_to_string acc

let password_hasher ?(iterations = 210_000) () =
  if iterations <= 0 then invalid_arg "Fennec.Accounts.Password.password_hasher: iterations must be positive";
  let hash ~password =
    let salt = secure_random 16 in
    let derived = pbkdf2_sha256 ~password ~salt ~iterations in
    String.concat "$" [ "pbkdf2-sha256"; string_of_int iterations; b64e salt; b64e derived ]
  in
  let verify ~password ~hash =
    match String.split_on_char '$' hash with
    | [ "pbkdf2-sha256"; iter_s; salt_s; derived_s ] -> (
      match (int_of_string_opt iter_s, b64d salt_s, b64d derived_s) with
      | Some iterations, Some salt, Some expected when iterations > 0 ->
        constant_eq expected (pbkdf2_sha256 ~password ~salt ~iterations)
      | _ -> false)
    | _ -> false
  in
  { hash; verify }

let hasher = password_hasher

type validation_error =
  | Too_short of int
  | Too_long of int
  | Missing_lowercase
  | Missing_uppercase
  | Missing_digit
  | Missing_symbol
  | Contains_email
  | Contains_username
  | Banned

let string_of_validation_error = function
  | Too_short n -> Printf.sprintf "Password must be at least %d characters" n
  | Too_long n -> Printf.sprintf "Password must be at most %d characters" n
  | Missing_lowercase -> "Password must contain a lowercase letter"
  | Missing_uppercase -> "Password must contain an uppercase letter"
  | Missing_digit -> "Password must contain a digit"
  | Missing_symbol -> "Password must contain a symbol"
  | Contains_email -> "Password must not contain the email address"
  | Contains_username -> "Password must not contain the username"
  | Banned -> "Password is too common"

let describe_errors = function
  | [] -> ""
  | errors -> String.concat "; " (List.map string_of_validation_error errors)

type policy = {
  min_length : int;
  max_length : int option;
  require_lowercase : bool;
  require_uppercase : bool;
  require_digit : bool;
  require_symbol : bool;
  reject_email : bool;
  reject_username : bool;
  banned : string list;
}

let default_banned =
  [
    "password";
    "password1";
    "123456";
    "12345678";
    "qwerty";
    "letmein";
    "admin";
    "welcome";
    "iloveyou";
    "changeme";
  ]

let normalize s = String.lowercase_ascii (String.trim s)

let policy ?(min_length = 8) ?(max_length = Some 1024) ?(require_lowercase = false)
    ?(require_uppercase = false) ?(require_digit = false) ?(require_symbol = false)
    ?(reject_email = true) ?(reject_username = true) ?(banned = default_banned) () =
  if min_length <= 0 then invalid_arg "Fennec.Accounts.Password.policy: min_length must be positive";
  (match max_length with
  | Some n when n < min_length ->
    invalid_arg "Fennec.Accounts.Password.policy: max_length must be at least min_length"
  | _ -> ());
  {
    min_length;
    max_length;
    require_lowercase;
    require_uppercase;
    require_digit;
    require_symbol;
    reject_email;
    reject_username;
    banned = List.filter_map (fun s -> let s = normalize s in if s = "" then None else Some s) banned;
  }

let default_policy = policy ()

let strict_policy =
  policy ~min_length:12 ~require_lowercase:true ~require_uppercase:true ~require_digit:true
    ~require_symbol:true ()

let contains pred s =
  let found = ref false in
  String.iter (fun c -> if pred c then found := true) s;
  !found

let contains_lower s = contains (function 'a' .. 'z' -> true | _ -> false) s
let contains_upper s = contains (function 'A' .. 'Z' -> true | _ -> false) s
let contains_digit s = contains (function '0' .. '9' -> true | _ -> false) s

let contains_symbol s =
  contains
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> false
      | c -> Char.code c >= 0x21 && Char.code c <= 0x7e)
    s

let string_contains ~needle haystack =
  let n = String.length needle in
  let h = String.length haystack in
  n > 0
  && h >= n
  &&
  let rec loop i =
    i + n <= h && (String.sub haystack i n = needle || loop (i + 1))
  in
  loop 0

let validate ?email ?username ?(policy = default_policy) password =
  let lower = normalize password in
  let add_if cond err acc = if cond then err :: acc else acc in
  let errors =
    []
    |> add_if (String.length password < policy.min_length) (Too_short policy.min_length)
    |> add_if
         (match policy.max_length with Some max -> String.length password > max | None -> false)
         (Too_long (Option.value ~default:0 policy.max_length))
    |> add_if (policy.require_lowercase && not (contains_lower password)) Missing_lowercase
    |> add_if (policy.require_uppercase && not (contains_upper password)) Missing_uppercase
    |> add_if (policy.require_digit && not (contains_digit password)) Missing_digit
    |> add_if (policy.require_symbol && not (contains_symbol password)) Missing_symbol
    |> add_if
         (policy.reject_email
         &&
         match Option.map normalize email with
         | Some email when email <> "" -> string_contains ~needle:email lower
         | _ -> false)
         Contains_email
    |> add_if
         (policy.reject_username
         &&
         match Option.map normalize username with
         | Some username when username <> "" -> string_contains ~needle:username lower
         | _ -> false)
         Contains_username
    |> add_if (List.exists (String.equal lower) policy.banned) Banned
    |> List.rev
  in
  match errors with [] -> Ok () | errors -> Error errors

let is_valid ?email ?username ?policy password =
  match validate ?email ?username ?policy password with Ok () -> true | Error _ -> false

(* ---- inline tests ---- *)

let raises_invalid_arg f = match f () with exception Invalid_argument _ -> true | _ -> false

let has_error pred = function
  | Ok () -> false
  | Error errors -> List.exists pred errors

let%test "password_hasher verifies matching passwords and rejects wrong ones" =
  let h = password_hasher ~iterations:2 () in
  let hash = h.hash ~password:"correct horse battery staple" in
  h.verify ~password:"correct horse battery staple" ~hash && not (h.verify ~password:"wrong" ~hash)

let%test "password_hasher rejects invalid hashes and iterations" =
  let h = password_hasher ~iterations:2 () in
  (not (h.verify ~password:"pw" ~hash:"not-a-hash"))
  && (not (h.verify ~password:"pw" ~hash:"pbkdf2-sha256$0$salt$derived"))
  && raises_invalid_arg (fun () -> password_hasher ~iterations:0 ())

let%test "policy constructor validates bounds" =
  raises_invalid_arg (fun () -> policy ~min_length:0 ())
  && raises_invalid_arg (fun () -> policy ~min_length:10 ~max_length:(Some 9) ())

let%test "default policy accepts long non-common passwords" =
  validate "correct horse battery staple" = Ok ()

let%test "default policy rejects short and common passwords" =
  has_error (function Too_short 8 -> true | _ -> false) (validate "short")
  && has_error (function Banned -> true | _ -> false) (validate "password")

let%test "policy can reject email and username substrings" =
  has_error (function Contains_email -> true | _ -> false)
    (validate ~email:"ADA@example.com" "x-ada@example.com-y")
  && has_error (function Contains_username -> true | _ -> false)
       (validate ~username:"AdaLovelace" "x-adalovelace-y")

let%test "strict policy requires character classes" =
  match validate ~policy:strict_policy "alllowercasepassword" with
  | Ok () -> false
  | Error errors ->
    List.mem Missing_uppercase errors
    && List.mem Missing_digit errors
    && List.mem Missing_symbol errors
    && not (List.mem Missing_lowercase errors)

let%test "strict policy accepts mixed strong password" =
  validate ~policy:strict_policy "CorrectHorse1!" = Ok ()

let%test "custom policy can disable email and username rejection" =
  let policy = policy ~reject_email:false ~reject_username:false () in
  validate ~policy ~email:"ada@example.com" ~username:"ada" "ada@example.com-ada-long" = Ok ()

let%test "describe_errors is stable and concise" =
  describe_errors [ Too_short 8; Missing_digit ] =
  "Password must be at least 8 characters; Password must contain a digit"
