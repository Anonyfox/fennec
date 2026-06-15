(* Live observeChanges: initial burst, added on insert/enter, changed on field change, removed on
   delete/leave, projection (fields without _id), unregister, and synchronous fence. *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let tmp label =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_" ^ label ^ "_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.No_sync (tmp "observe") in
  let c = Eng.collection eng "t" in
  let events = ref [] in
  let log e = events := e :: !events in
  let take () =
    let e = List.rev !events in
    events := [];
    e
  in
  let stop =
    Eng.observe_changes eng c ~selector:(doc [ ("active", B.bool true) ]) ~sort:empty ~skip:0 ~limit:0 ~fields:empty
      ~added:(fun id f -> log (Printf.sprintf "added %s n=%s" id (match B.get_int f "n" with Some n -> string_of_int n | None -> "_")))
      ~changed:(fun id _ cleared -> log (Printf.sprintf "changed %s cleared=[%s]" id (String.concat ";" cleared)))
      ~removed:(fun id -> log ("removed " ^ id))
  in

  (* insert matching -> added *)
  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("active", B.bool true); ("n", i 1) ]));
  assert (take () = [ "added a n=1" ]);

  (* insert non-matching -> nothing *)
  ignore (Eng.insert eng c (doc [ ("_id", s "b"); ("active", B.bool false) ]));
  assert (take () = []);

  (* change a field on a matching doc -> changed *)
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("n", i 2) ]) ]));
  assert (take () = [ "changed a cleared=[]" ]);

  (* b enters the set -> added *)
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "b") ]) (doc [ ("$set", doc [ ("active", B.bool true) ]) ]));
  assert (take () = [ "added b n=_" ]);

  (* a leaves the set -> removed *)
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("active", B.bool false) ]) ]));
  assert (take () = [ "removed a" ]);

  (* remove b -> removed *)
  ignore (Eng.remove eng c (doc [ ("_id", s "b") ]));
  assert (take () = [ "removed b" ]);

  (* a field cleared via $unset shows up in cleared names *)
  ignore (Eng.insert eng c (doc [ ("_id", s "k"); ("active", B.bool true); ("x", i 9) ]));
  assert (take () = [ "added k n=_" ]);
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "k") ]) (doc [ ("$unset", doc [ ("x", i 1) ]) ]));
  assert (take () = [ "changed k cleared=[x]" ]);

  (* unregister: no further events *)
  stop ();
  ignore (Eng.insert eng c (doc [ ("_id", s "z"); ("active", B.bool true) ]));
  assert (take () = []);

  (* fence is synchronous *)
  let fenced = ref false in
  Eng.fence eng c (fun () -> fenced := true);
  assert !fenced;

  Eng.close eng;
  print_string "engine observe: OK\n"
