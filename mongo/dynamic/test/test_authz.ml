(* Authorization logic (pure): the per-database role decision + the command classification that gate the
   wire endpoint. This is the security boundary, so it's tested exhaustively here (always — no driver
   needed); enforcement end-to-end is in fennec/pulse/mongo/test/test_wire_server.ml. *)
module M = Fennec_mongo_dynamic
module Role = M.Role

let () =
  (* Root: everything *)
  assert (Role.allows ~write:true ~db:"any" Role.Root);
  assert (Role.allows ~write:false ~db:"any" Role.Root);
  (* Read: reads on its database only, never writes *)
  assert (Role.allows ~write:false ~db:"shop" (Role.Read "shop"));
  assert (not (Role.allows ~write:true ~db:"shop" (Role.Read "shop")));
  assert (not (Role.allows ~write:false ~db:"other" (Role.Read "shop")));
  (* Read_write: reads + writes on its database only *)
  assert (Role.allows ~write:true ~db:"shop" (Role.Read_write "shop"));
  assert (Role.allows ~write:false ~db:"shop" (Role.Read_write "shop"));
  assert (not (Role.allows ~write:true ~db:"other" (Role.Read_write "shop")));
  (* the "*" wildcard matches any database *)
  assert (Role.allows ~write:true ~db:"anything" (Role.Read_write "*"));
  assert (Role.allows ~write:false ~db:"anything" (Role.Read "*"));
  assert (not (Role.allows ~write:true ~db:"anything" (Role.Read "*")));

  (* command classification: every MUTATING command MUST be `Write, or it would bypass authorization *)
  let acc c = M.command_access c in
  List.iter
    (fun c -> assert (acc c = `Write))
    [ "insert"; "update"; "delete"; "findAndModify"; "createIndexes"; "dropIndexes"; "drop"; "create"; "dropDatabase" ];
  List.iter
    (fun c -> assert (acc c = `Read))
    [ "find"; "getMore"; "count"; "distinct"; "aggregate"; "explain"; "listCollections"; "listIndexes" ];
  List.iter (fun c -> assert (acc c = `Exempt)) [ "hello"; "ping"; "saslStart"; "saslContinue"; "logout"; "buildInfo"; "listDatabases" ];

  print_string "authz: OK\n"
