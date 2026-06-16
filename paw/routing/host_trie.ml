(* See host_trie.mli. A reversed-label trie for O(1) exact and O(depth) suffix host matching.

   Hosts are split on '.', reversed, and walked from the TLD down. Each node uses a Hashtbl for
   O(1) child lookup. Two kinds of match terminate at a node:

     - EXACT: all labels consumed and the node carries payload(s). "admin.acme.com" = the path
       com → acme → admin, payload on the "admin" node.
     - WILDCARD (suffix): the node carries wildcard payload(s) AND there are remaining labels to
       consume. "*.acme.com" = the path com → acme, wildcard on the "acme" node. Matches
       "x.acme.com" (1 remaining label) and "a.b.acme.com" (2 remaining) but NOT "acme.com"
       (0 remaining — the "one or more leading labels" requirement).

   {!lookup_all} returns EVERY matching endpoint, most-specific-first: the node's exact payloads,
   then wildcards from the DEEPEST node up (a deeper "*.api.acme.com" before a shallower
   "*.acme.com"), each level in declaration (insertion) order. {!lookup} keeps the old single-best
   shape (the head of {!lookup_all}). Overlap is first-class — a node may carry several endpoints
   (the router validated names, not pattern uniqueness), so each marker is a list.

   Built once at startup from a pattern list (in declaration order), so construction is imperative
   and lookup is pure and allocation-light. *)

type 'ep node = {
  children : (string, 'ep node) Hashtbl.t;
  mutable payload : 'ep list; (* exact matchers terminating here, in declaration order *)
  mutable wildcard : 'ep list; (* "*."-suffix matchers anchored here, in declaration order *)
}

type 'ep t = { root : 'ep node }

(* ──── make_node ──── *)

let make_node () = { children = Hashtbl.create 4; payload = []; wildcard = [] }

(* ──── split_labels ──── *)

let split_labels (s : string) : string list =
  String.split_on_char '.' s |> List.filter (fun l -> l <> "")

(* ──── build ──── *)

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
        node.payload <- node.payload @ [ ep ]
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
        node.wildcard <- node.wildcard @ [ ep ])
    patterns;
  { root }

(* ──── lookup_all ──── *)

(* Every endpoint matching [host], most-specific-first: the exact payloads at the fully-consumed
   node, then the wildcard payloads from the deepest matching node up to the shallowest. We prepend
   each node's wildcards as we descend, so the deepest (visited last) ends up at the FRONT — i.e.
   "*.api.acme.com" before "*.acme.com". Each list is already in declaration order. *)
let lookup_all (t : 'ep t) ~(host : string) : 'ep list =
  let host = Host_pattern.normalize host in
  let labels = List.rev (split_labels host) in
  let rec walk node labels wilds =
    (* a wildcard here matches only with ≥1 remaining label (the "*." requires a leading label) *)
    let wilds = if labels <> [] then node.wildcard @ wilds else wilds in
    match labels with
    | [] -> node.payload @ wilds (* exact (most specific) first, then suffixes deepest-first *)
    | label :: rest -> (
      match Hashtbl.find_opt node.children label with
      | Some child -> walk child rest wilds
      | None -> wilds (* dead end: the suffixes collected so far, deepest-first *))
  in
  walk t.root labels []

(* ──── lookup ──── *)

(* the single most-specific match (the head of {!lookup_all}); kept for callers that want one *)
let lookup (t : 'ep t) ~(host : string) : 'ep option =
  match lookup_all t ~host with x :: _ -> Some x | [] -> None

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

(* lookup_all — overlap + ordering *)
let%test "all: two exacts at same host, declaration order" =
  let t = build [ (pat "app.com", "auth"); (pat "app.com", "public") ] in
  lookup_all t ~host:"app.com" = [ "auth"; "public" ]
let%test "all: exact before wildcard" =
  let t = build [ (pat "*.acme.com", "wild"); (pat "api.acme.com", "exact") ] in
  lookup_all t ~host:"api.acme.com" = [ "exact"; "wild" ]
let%test "all: deeper wildcard before shallower" =
  let t = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  lookup_all t ~host:"x.api.acme.com" = [ "deep"; "shallow" ]
let%test "all: shallow only at its level" =
  let t = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep") ] in
  lookup_all t ~host:"x.acme.com" = [ "shallow" ]
let%test "all: two wildcards at the same node, declaration order" =
  let t = build [ (pat "*.acme.com", "a"); (pat "*.acme.com", "b") ] in
  lookup_all t ~host:"x.acme.com" = [ "a"; "b" ]
let%test "all: exact + same-level wildcards, exact first then decl order" =
  let t = build [ (pat "*.acme.com", "w1"); (pat "api.acme.com", "e"); (pat "*.acme.com", "w2") ] in
  lookup_all t ~host:"api.acme.com" = [ "e"; "w1"; "w2" ]
let%test "all: no match is empty" =
  let t = build [ (pat "acme.com", "a") ] in
  lookup_all t ~host:"other.org" = []
let%test "all: full chain exact→deep→shallow" =
  let t = build [ (pat "*.acme.com", "shallow"); (pat "*.api.acme.com", "deep"); (pat "x.api.acme.com", "exact") ] in
  lookup_all t ~host:"x.api.acme.com" = [ "exact"; "deep"; "shallow" ]
