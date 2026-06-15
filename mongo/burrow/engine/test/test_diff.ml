(* Differential test: an identical random workload (inserts / multi-updates / removes / queries) is
   applied to Burrow and to minimongo (the reference in-memory engine), and their full collection state
   and query results must agree at every checkpoint. Burrow carries secondary indexes on a/b/arr, so
   this also validates the index access paths (equality, range, multikey) against the scan reference.

   Constraints that keep the comparison sound: documents carry explicit unique _ids (so both stores
   hold identical documents); updates are always multi (a non-multi update over a multi-match selector
   would pick an implementation-defined document and legitimately diverge); fields are int/string/bool/
   array only (no floats, so structural [=] on canonicalized lists is valid). *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module MM = Minimongo
module B = Bson

let tmp label =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_" ^ label ^ "_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []
let canon docs = List.sort B.compare docs
let bstrs = [| "x"; "y"; "z" |]

let () =
  Eio_main.run @@ fun _env ->
  let eng = Eng.open_ ~durability:S.No_sync (tmp "diff") in
  let c = Eng.collection eng "t" in
  let mm = MM.create () in
  Eng.ensure_index eng c ~name:"a_1" ~keys:(doc [ ("a", i 1) ]) ~unique:false;
  Eng.ensure_index eng c ~name:"b_1" ~keys:(doc [ ("b", i 1) ]) ~unique:false;
  Eng.ensure_index eng c ~name:"arr_1" ~keys:(doc [ ("arr", i 1) ]) ~unique:false;
  Eng.ensure_index eng c ~name:"a_b_1" ~keys:(doc [ ("a", i 1); ("b", i 1) ]) ~unique:false;

  Random.init 0xD1FF;
  let next_id = ref 0 in

  let rand_doc id =
    let base = [ ("_id", s id); ("a", i (Random.int 6)); ("b", s bstrs.(Random.int 3)) ] in
    if Random.bool () then
      doc (base @ [ ("arr", B.array (List.filter_map (fun v -> if Random.bool () then Some (i v) else None) [ 1; 2; 3 ])) ])
    else doc base
  in

  let rand_in () = B.array (List.filter_map (fun v -> if Random.bool () then Some (i v) else None) [ 0; 1; 2; 3; 4; 5 ]) in
  let rand_selector () =
    match Random.int 9 with
    | 0 -> empty
    | 1 -> doc [ ("a", i (Random.int 6)) ]
    | 2 -> doc [ ("b", s bstrs.(Random.int 3)) ]
    | 3 -> doc [ ("a", doc [ ("$gte", i (Random.int 6)) ]) ]
    | 4 -> doc [ ("a", doc [ ("$gt", i 1); ("$lt", i 5) ]) ]
    | 5 -> doc [ ("arr", i (1 + Random.int 3)) ]
    | 6 -> doc [ ("a", i (Random.int 6)); ("b", s bstrs.(Random.int 3)) ] (* compound a+b *)
    | 7 -> doc [ ("a", doc [ ("$in", rand_in ()) ]) ] (* $in on an indexed field *)
    | _ -> doc [ ("a", doc [ ("$gte", i 2) ]); ("b", s "x") ]
  in

  let both_find sel =
    ( canon (Eng.find eng c ~selector:sel ~sort:empty ~skip:0 ~limit:0 ~fields:empty),
      canon (MM.fetch (MM.find mm ~selector:sel ())) )
  in
  let check_query sel =
    let b, m = both_find sel in
    if b <> m then begin
      Printf.eprintf "QUERY DIVERGE sel=%s  burrow=%d  minimongo=%d\n%!" (B.to_string sel) (List.length b) (List.length m);
      List.iter (fun d -> if not (List.mem d m) then Printf.eprintf "  only burrow: %s\n%!" (B.to_string d)) b;
      List.iter (fun d -> if not (List.mem d b) then Printf.eprintf "  only mmongo: %s\n%!" (B.to_string d)) m;
      assert false
    end
  in
  let check_all () = check_query empty in

  let step () =
    match Random.int 10 with
    | 0 | 1 | 2 | 3 ->
      let id = Printf.sprintf "k%d" !next_id in
      incr next_id;
      let d = rand_doc id in
      ignore (Eng.insert eng c d);
      ignore (MM.insert mm d);
      "insert " ^ B.to_string d
    | 4 | 5 ->
      let sel = rand_selector () in
      let modi = if Random.bool () then doc [ ("$set", doc [ ("b", s "w") ]) ] else doc [ ("$inc", doc [ ("a", i 1) ]) ] in
      ignore (Eng.update eng c ~multi:true ~upsert:false sel modi);
      ignore (MM.update mm ~multi:true ~upsert:false sel modi);
      Printf.sprintf "update sel=%s mod=%s" (B.to_string sel) (B.to_string modi)
    | 6 ->
      let sel = rand_selector () in
      ignore (Eng.remove eng c sel);
      ignore (MM.remove mm sel);
      "remove sel=" ^ B.to_string sel
    | _ ->
      let sel = rand_selector () in
      "query sel=" ^ B.to_string sel
  in
  (* probe a battery of index-using queries every step, so an index divergence is caught at the
     operation that causes it (not when a much later query happens to hit the lost entry) *)
  let probes =
    [ empty; doc [ ("arr", i 1) ]; doc [ ("arr", i 2) ]; doc [ ("arr", i 3) ];
      doc [ ("a", doc [ ("$gte", i 0) ]) ]; doc [ ("a", i 2) ]; doc [ ("b", s "x") ]; doc [ ("b", s "w") ];
      doc [ ("a", doc [ ("$in", B.array [ i 0; i 2; i 4 ]) ]) ]; (* $in path *)
      doc [ ("a", i 3); ("b", s "y") ] (* compound path *) ]
  in
  (* ordered parity (sequence, not just set): sort on the indexed [a] with an [_id] tiebreak (a TOTAL
     order, so it's deterministic across engines regardless of storage order), asc + desc, paginated and
     filtered. This is what makes a later sort-via-index optimization catchable. *)
  let ord ?(sel = empty) sort skip limit =
    ( Eng.find eng c ~selector:sel ~sort ~skip ~limit ~fields:empty,
      MM.fetch (MM.find mm ~selector:sel ~sort ~skip ~limit ()) )
  in
  let check_ord ?(sel = empty) sort skip limit =
    let b, m = ord ~sel sort skip limit in
    if b <> m then begin
      Printf.eprintf "ORDER DIVERGE sort=%s sel=%s burrow=%d minimongo=%d\n%!" (B.to_string sort) (B.to_string sel)
        (List.length b) (List.length m);
      assert false
    end
  in
  let asc = doc [ ("a", i 1); ("_id", i 1) ] and desc = doc [ ("a", i (-1)); ("_id", i 1) ] in
  for n = 1 to 4000 do
    let desc_str = step () in
    List.iter
      (fun sel ->
        let b, m = both_find sel in
        if b <> m then begin
          Printf.eprintf "DIVERGE after step %d: %s\n  probe sel=%s  burrow=%d  minimongo=%d\n%!"
            n desc_str (B.to_string sel) (List.length b) (List.length m);
          List.iter (fun d -> if not (List.mem d m) then Printf.eprintf "  only burrow: %s\n%!" (B.to_string d)) b;
          List.iter (fun d -> if not (List.mem d b) then Printf.eprintf "  only mmongo: %s\n%!" (B.to_string d)) m;
          assert false
        end)
      probes;
    if n mod 11 = 0 then begin
      check_ord asc (n mod 7) 5;
      check_ord desc 0 (1 + (n mod 9));
      check_ord ~sel:(doc [ ("a", doc [ ("$gte", i 2) ]) ]) asc 0 0
    end
  done;
  check_all ();

  (* final-state ordered parity across more sorts: _id, indexed a (asc/desc, paginated), string b,
     and a filtered+sorted (range selector + sort), all with an _id tiebreak for determinism *)
  check_ord (doc [ ("_id", i 1) ]) 0 0;
  check_ord asc 5 10;
  check_ord desc 3 7;
  check_ord (doc [ ("b", i 1); ("_id", i 1) ]) 0 0;
  check_ord ~sel:(doc [ ("a", doc [ ("$gt", i 1); ("$lt", i 5) ]) ]) asc 2 5;

  (* distinct + count parity across a battery of selectors *)
  for _ = 1 to 50 do
    let sel = rand_selector () in
    assert (canon (Eng.distinct eng c ~key:"a" ~selector:sel) = canon (MM.distinct mm ~key:"a" ~selector:sel ()));
    assert (Eng.count eng c ~selector:sel = MM.count (MM.find mm ~selector:sel ()))
  done;

  Eng.close eng;
  print_string "differential vs minimongo: OK\n"
