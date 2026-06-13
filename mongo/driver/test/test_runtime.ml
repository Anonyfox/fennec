module Runtime = Fennec_mongo_driver.Runtime

let with_env name value f =
  let old = Sys.getenv_opt name in
  (match value with
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name "");
  Fun.protect f ~finally:(fun () ->
      match old with
      | Some value -> Unix.putenv name value
      | None -> Unix.putenv name "")

let () =
  assert (Runtime.mongo_url_env = "MONGO_URL");
  assert (Runtime.memory_url = ":memory:");
  assert (Runtime.default_db = "fennec");
  assert (Runtime.is_memory_url ":memory:");
  assert (Runtime.is_memory_url " :memory: ");
  assert (not (Runtime.is_memory_url "mongodb://127.0.0.1:27017"));
  with_env "MONGO_URL" None (fun () ->
      assert (Runtime.url () = None);
      assert (Runtime.state () = Runtime.Missing));
  with_env "MONGO_URL" (Some "  ") (fun () ->
      assert (Runtime.url () = None);
      assert (Runtime.state () = Runtime.Missing));
  with_env "MONGO_URL" (Some " :memory: ") (fun () ->
      assert (Runtime.url () = Some ":memory:");
      assert (Runtime.state () = Runtime.Memory));
  with_env "MONGO_URL" (Some " mongodb://127.0.0.1:27017/fennec ") (fun () ->
      assert (Runtime.url () = Some "mongodb://127.0.0.1:27017/fennec"));
  with_env "FENNEC_DB" None (fun () -> assert (Runtime.db () = "fennec"));
  with_env "FENNEC_DB" (Some "  ") (fun () -> assert (Runtime.db () = "fennec"));
  with_env "FENNEC_DB" (Some " app_db ") (fun () -> assert (Runtime.db () = "app_db"));
  with_env "MONGO_URL" (Some "mongodb://127.0.0.1:27017") (fun () ->
      with_env "FENNEC_DB" (Some " app_db ") (fun () ->
          assert (Runtime.state () = Runtime.Mongo { uri = "mongodb://127.0.0.1:27017"; db = "app_db" })));
  assert (String.length (Runtime.unavailable_message ()) > 20)
