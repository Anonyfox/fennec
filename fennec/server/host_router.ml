(* See host_router.mli. The validated, sorted host->endpoint table. The ONLY way to obtain one is
   [build], which enforces every routing invariant once — names non-empty/clean/unique, every
   pattern parses, AT MOST ONE catch-all, no two endpoints claiming the same pattern — and sorts the
   specific patterns most-specific-first. So the server holds a table whose illegal states are
   unrepresentable, and [route] is a total walk: most-specific match, else the single default, else
   None (an unknown host with no "*" → the caller 404s, never a misroute to the wrong tenant). *)

module P = Host_pattern

type 'ep entry = { name : string; patterns : P.t list; ep : 'ep }

type 'ep t = {
  specific : (P.t * 'ep) list; (* non-Any patterns, sorted most-specific first *)
  default : 'ep option; (* the single "*" owner's payload, if any *)
  entries : 'ep entry list; (* as DECLARED (declaration order) — for dev port allocation + banner *)
}

type error =
  | Bad_name of string
  | Duplicate_name of string
  | No_patterns of string
  | Bad_pattern of string * string
  | Multiple_catch_all of string list
  | Conflicting_pattern of string * string * string

let name_ok n = n <> "" && not (String.exists (fun c -> c = ' ' || c = '=' || c = '\t' || c = '\n') n)

let parse_entry (name, raws, ep) =
  if not (name_ok name) then Error (Bad_name name)
  else if raws = [] then Error (No_patterns name)
  else
    let rec go acc = function
      | [] -> Ok { name; patterns = List.rev acc; ep }
      | r :: rest -> ( match P.of_string r with Ok p -> go (p :: acc) rest | Error msg -> Error (Bad_pattern (name, msg)))
    in
    go [] raws

let build (inputs : (string * string list * 'ep) list) : ('ep t, error) result =
  let rec parse_all acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> ( match parse_entry x with Ok e -> parse_all (e :: acc) rest | Error e -> Error e)
  in
  match parse_all [] inputs with
  | Error e -> Error e
  | Ok entries -> (
    let rec dup_name seen = function [] -> None | e :: rest -> if List.mem e.name seen then Some e.name else dup_name (e.name :: seen) rest in
    match dup_name [] entries with
    | Some n -> Error (Duplicate_name n)
    | None -> (
      let catch_alls = List.filter_map (fun e -> if List.mem P.Any e.patterns then Some e.name else None) entries in
      match catch_alls with
      | _ :: _ :: _ -> Error (Multiple_catch_all catch_alls)
      | _ -> (
        (* every (non-catch-all) pattern with its owning endpoint *)
        let owned = List.concat_map (fun e -> List.filter_map (fun p -> if p = P.Any then None else Some (p, e)) e.patterns) entries in
        let conflict =
          let rec find = function
            | [] -> None
            | (p, e) :: rest -> ( match List.find_opt (fun (p2, e2) -> p2 = p && e2.name <> e.name) rest with Some (_, e2) -> Some (P.to_string p, e.name, e2.name) | None -> find rest)
          in
          find owned
        in
        match conflict with
        | Some (pat, a, b) -> Error (Conflicting_pattern (pat, a, b))
        | None ->
          let specific =
            owned |> List.map (fun (p, e) -> (p, e.ep))
            (* most-specific first; stable_sort keeps declaration order among equal-specificity ties *)
            |> List.stable_sort (fun (a, _) (b, _) -> compare (P.specificity b) (P.specificity a))
          in
          let default = List.find_map (fun e -> if List.mem P.Any e.patterns then Some e.ep else None) entries in
          Ok { specific; default; entries })))

let route (t : 'ep t) ~(host : string) : 'ep option =
  match List.find_opt (fun (p, _) -> P.matches p ~host) t.specific with Some (_, ep) -> Some ep | None -> t.default

let entries (t : 'ep t) = t.entries

let describe_error = function
  | Bad_name n -> Printf.sprintf "endpoint name %S is empty or contains whitespace/'='" n
  | Duplicate_name n -> Printf.sprintf "two endpoints share the name %S" n
  | No_patterns n -> Printf.sprintf "endpoint %S declares no host patterns" n
  | Bad_pattern (n, msg) -> Printf.sprintf "endpoint %S: %s" n msg
  | Multiple_catch_all names -> Printf.sprintf "more than one catch-all \"*\" (%s) — only one endpoint may be the default" (String.concat ", " names)
  | Conflicting_pattern (pat, a, b) -> Printf.sprintf "endpoints %S and %S both claim the host %S" a b pat
