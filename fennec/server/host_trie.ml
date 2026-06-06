(* See host_trie.mli. A reversed-label trie for O(1) exact and O(depth) suffix host matching.

   Hosts are split on '.', reversed, and walked from the TLD down. Each node uses a Hashtbl for
   O(1) child lookup. Two kinds of match terminate at a node:

     - EXACT: all labels consumed and the node carries a payload. "admin.acme.com" = the path
       com → acme → admin, payload on the "admin" node.
     - WILDCARD (suffix): the node carries a wildcard marker AND there are remaining labels to
       consume. "*.acme.com" = the path com → acme, wildcard on the "acme" node. Matches
       "x.acme.com" (1 remaining label) and "a.b.acme.com" (2 remaining) but NOT "acme.com"
       (0 remaining — the "one or more leading labels" requirement).

   When multiple wildcards could match (e.g. "*.acme.com" and "*.api.acme.com" for
   "x.api.acme.com"), the DEEPEST one wins — naturally, because the walk goes as deep as it can
   before falling back. An exact match always beats any wildcard.

   Built once at startup from a pre-validated pattern list (Host_router.build already rejected
   conflicts), so construction is imperative and lookup is pure. *)

type 'ep node = {
  children : (string, 'ep node) Hashtbl.t;
  mutable payload : 'ep option;
  mutable wildcard : 'ep option;
}

type 'ep t = { root : 'ep node }

let make_node () = { children = Hashtbl.create 4; payload = None; wildcard = None }

let split_labels (s : string) : string list =
  String.split_on_char '.' s |> List.filter (fun l -> l <> "")

let build (patterns : (Host_pattern.t * 'ep) list) : 'ep t =
  let root = make_node () in
  List.iter
    (fun (pat, ep) ->
      match pat with
      | Host_pattern.Any -> () (* held outside the trie by Host_router *)
      | Host_pattern.Exact host ->
        let labels = List.rev (split_labels host) in
        let node =
          List.fold_left
            (fun n label ->
              match Hashtbl.find_opt n.children label with
              | Some child -> child
              | None ->
                let child = make_node () in
                Hashtbl.replace n.children label child;
                child)
            root labels
        in
        node.payload <- Some ep
      | Host_pattern.Suffix suf ->
        (* suf is ".acme.com" (leading dot); strip it to get the label path *)
        let host = if String.length suf > 0 && suf.[0] = '.' then String.sub suf 1 (String.length suf - 1) else suf in
        let labels = List.rev (split_labels host) in
        let node =
          List.fold_left
            (fun n label ->
              match Hashtbl.find_opt n.children label with
              | Some child -> child
              | None ->
                let child = make_node () in
                Hashtbl.replace n.children label child;
                child)
            root labels
        in
        node.wildcard <- Some ep)
    patterns;
  { root }

let lookup (t : 'ep t) ~(host : string) : 'ep option =
  let host = Host_pattern.normalize host in
  let labels = List.rev (split_labels host) in
  let rec walk node labels best_wildcard =
    (* if this node has a wildcard AND there are remaining labels to consume, it's a candidate
       (the "one or more leading labels" requirement for *.suffix patterns) *)
    let best = if labels <> [] then (match node.wildcard with Some _ as w -> w | None -> best_wildcard) else best_wildcard in
    match labels with
    | [] ->
      (* consumed all labels: an exact match (payload) wins over any remembered wildcard *)
      (match node.payload with Some _ as p -> p | None -> best)
    | label :: rest -> (
      match Hashtbl.find_opt node.children label with
      | Some child -> walk child rest best
      | None -> best (* can't descend further; return the deepest wildcard we saw *))
  in
  walk t.root labels None

(* -- inline tests --------------------------------------------------------- *)

let pat s = Result.get_ok (Host_pattern.of_string s)

(* exact matching *)
let%test "exact match" =
  let t = build [ (pat "acme.com", "acme"); (pat "admin.acme.com", "admin") ] in
  lookup t ~host:"acme.com" = Some "acme"
let%test "exact match (subdomain)" =
  let t = build [ (pat "acme.com", "acme"); (pat "admin.acme.com", "admin") ] in
  lookup t ~host:"admin.acme.com" = Some "admin"
let%test "exact miss" =
  let t = build [ (pat "acme.com", "acme"); (pat "admin.acme.com", "admin") ] in
  lookup t ~host:"other.com" = None
let%test "partial match not a hit" =
  let t = build [ (pat "acme.com", "acme"); (pat "admin.acme.com", "admin") ] in
  lookup t ~host:"x.admin.acme.com" = None
let%test "case-insensitive" =
  let t = build [ (pat "acme.com", "acme") ] in
  lookup t ~host:"ACME.COM" = Some "acme"
let%test "host with :port" =
  let t = build [ (pat "acme.com", "acme") ] in
  lookup t ~host:"acme.com:4000" = Some "acme"
let%test "empty host" =
  let t = build [ (pat "acme.com", "acme") ] in
  lookup t ~host:"" = None

(* wildcard / suffix matching *)
let%test "wildcard matches subdomain" =
  let tw = build [ (pat "*.acme.com", "wild") ] in
  lookup tw ~host:"api.acme.com" = Some "wild"
let%test "wildcard matches deep" =
  let tw = build [ (pat "*.acme.com", "wild") ] in
  lookup tw ~host:"a.b.acme.com" = Some "wild"
let%test "wildcard needs >=1 label" =
  let tw = build [ (pat "*.acme.com", "wild") ] in
  lookup tw ~host:"acme.com" = None
let%test "wildcard rejects other base" =
  let tw = build [ (pat "*.acme.com", "wild") ] in
  lookup tw ~host:"api.other.com" = None

(* precedence *)
let%test "exact beats wildcard" =
  let tp = build [ (pat "admin.acme.com", "exact"); (pat "*.acme.com", "wild") ] in
  lookup tp ~host:"admin.acme.com" = Some "exact"
let%test "non-exact falls to wildcard" =
  let tp = build [ (pat "admin.acme.com", "exact"); (pat "*.acme.com", "wild") ] in
  lookup tp ~host:"api.acme.com" = Some "wild"
let%test "deeper wildcard wins" =
  let td = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  lookup td ~host:"x.api.acme.com" = Some "deep"
let%test "shallower catches its level" =
  let td = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  lookup td ~host:"x.acme.com" = Some "shallow"
let%test "deep base matches shallow" =
  let td = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  lookup td ~host:"api.acme.com" = Some "shallow"

(* mixed exact + wildcards *)
let%test "mixed: root exact" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"acme.com" = Some "root"
let%test "mixed: admin exact" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"admin.acme.com" = Some "admin"
let%test "mixed: random -> catch" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"random.acme.com" = Some "catch"
let%test "mixed: api sub -> api_catch" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"x.api.acme.com" = Some "api_catch"
let%test "mixed: api base -> catch" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"api.acme.com" = Some "catch"
let%test "mixed: unrelated -> None" =
  let tm = build [ (pat "acme.com", "root"); (pat "admin.acme.com", "admin"); (pat "*.acme.com", "catch"); (pat "*.api.acme.com", "api_catch") ] in
  lookup tm ~host:"example.org" = None

(* edge cases *)
let%test "empty trie -> None" =
  let te = build [] in lookup te ~host:"anything.com" = None
let%test "single-label TLD mismatch" =
  let ts = build [ (pat "a.com", "a") ] in lookup ts ~host:"b.com" = None
let%test "trailing dot in host" =
  let ts = build [ (pat "a.com", "a") ] in lookup ts ~host:"a.com." = Some "a"
