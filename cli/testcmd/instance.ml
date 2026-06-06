(* Per-suite isolated instances — the core of deterministic parallel testing against stateful
   servers. Each suite gets its OWN port block and its OWN server process (and, later, its own
   database), so suites never share state and can run concurrently with reproducible results.

   Suite [i] takes the block starting at [base + i*stride]. The stride leaves headroom for the
   app's dev port block (a gateway port plus one forced port per endpoint), so neighbouring
   suites' blocks never overlap regardless of how many endpoints the app declares.

   The environment splits in two, mirroring the seam:
   - [server_env] spawns the app instance (its port, livereload off for determinism, and — in
     the future — an isolated database URL).
   - [suite_env] points the suite at that instance ([FENNEC_TEST_URL]). *)

module D = Fennec_core.Dev_proto

(* per-suite port block: generous headroom for gateway + endpoint ports, human-readable in
   logs (suite 0 → 7000s, suite 1 → 7100s, …) *)
let stride = 100

type t = {
  suite : string;
  port : int;
  url : string;
  server_env : (string * string) list;
  suite_env : (string * string) list;
}

let url_for port = Printf.sprintf "http://localhost:%d" port

let allocate ~base (suites : string list) : t list =
  List.mapi
    (fun i suite ->
      let port = base + (i * stride) in
      let url = url_for port in
      {
        suite;
        port;
        url;
        server_env = [ (D.env_port, string_of_int port); (D.env_dev_livereload, "0") ];
        suite_env = [ (D.env_test_url, url) ];
      })
    suites
