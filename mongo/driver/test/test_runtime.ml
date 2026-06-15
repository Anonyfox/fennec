(* The one MONGO_URL parser: :memory: / burrow:// / mongodb:// classified into the backend variant,
   with the burrow:// authority parsed into the optional wire-exposure spec, and the db taken from the
   trailing path segment. *)
module Runtime = Fennec_mongo_driver.Runtime

let with_env name value f =
  let old = Sys.getenv_opt name in
  (match value with Some value -> Unix.putenv name value | None -> Unix.putenv name "");
  Fun.protect f ~finally:(fun () -> Unix.putenv name (Option.value old ~default:""))

let mongo_url v f = with_env "MONGO_URL" (Some v) f

let () =
  assert (Runtime.mongo_url_env = "MONGO_URL");
  assert (Runtime.memory_url = ":memory:");
  assert (Runtime.default_db = "fennec");
  assert (Runtime.default_wire_port = 27017);

  (* missing / blank -> Missing *)
  with_env "MONGO_URL" None (fun () -> assert (Runtime.url () = None && Runtime.backend () = Runtime.Missing));
  with_env "MONGO_URL" (Some "   ") (fun () -> assert (Runtime.backend () = Runtime.Missing));

  (* :memory: (trimmed) *)
  mongo_url " :memory: " (fun () -> assert (Runtime.backend () = Runtime.Memory));

  (* burrow:// storage-only (no authority) -> path's trailing segment is the db, the rest is the base *)
  mongo_url "burrow:///var/lib/app" (fun () ->
      match Runtime.backend () with
      | Runtime.Burrow { base = "/var/lib"; db = "app"; expose = None } -> ()
      | _ -> assert false);

  (* burrow:// with a loopback authority + creds -> exposed, SCRAM, default port; mongosh-symmetric *)
  mongo_url "burrow://admin:s3cret@127.0.0.1:27017/srv/data/orders?tls=true&readonly=1" (fun () ->
      match Runtime.backend () with
      | Runtime.Burrow { base = "/srv/data"; db = "orders"; expose = Some e } ->
        assert (e.host = "127.0.0.1" && e.port = 27017);
        assert (e.user = Some "admin" && e.pass = Some "s3cret");
        assert (e.tls && e.read_only)
      | _ -> assert false);

  (* host-only authority -> default port 27017, no creds, no tls *)
  mongo_url "burrow://localhost/data/app" (fun () ->
      match Runtime.backend () with
      | Runtime.Burrow { db = "app"; expose = Some e; _ } ->
        assert (e.host = "localhost" && e.port = 27017 && e.user = None && not e.tls)
      | _ -> assert false);

  (* :port-only authority (bind all by host="") -> default bind, explicit port, unauth *)
  mongo_url "burrow://:31000/tmp/t" (fun () ->
      match Runtime.backend () with
      | Runtime.Burrow { db = "t"; expose = Some e; _ } -> assert (e.host = "127.0.0.1" && e.port = 31000 && e.user = None)
      | _ -> assert false);

  (* mongodb:// -> db from the URI path, else FENNEC_DB / default *)
  mongo_url "mongodb://u:p@host:27017/shop?replicaSet=rs0" (fun () ->
      match Runtime.backend () with
      | Runtime.Mongo { uri; db = "shop" } -> assert (uri = "mongodb://u:p@host:27017/shop?replicaSet=rs0")
      | _ -> assert false);
  mongo_url "mongodb://host:27017" (fun () ->
      with_env "FENNEC_DB" (Some "app_db") (fun () ->
          match Runtime.backend () with Runtime.Mongo { db = "app_db"; _ } -> () | _ -> assert false));

  (* FENNEC_DB fallback for :memory: db() *)
  with_env "FENNEC_DB" None (fun () -> assert (Runtime.db () = "fennec"));
  with_env "FENNEC_DB" (Some " app_db ") (fun () -> assert (Runtime.db () = "app_db"));

  assert (String.length (Runtime.unavailable_message ()) > 20);
  print_string "runtime MONGO_URL parser (:memory: / burrow:// / mongodb://): OK\n"
