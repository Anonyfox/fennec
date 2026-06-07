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

(* ──── tests ──── *)

let%test_unit "allocate: basic layout" =
  let open Fennec_hunt_unit in
  let inst = allocate ~base:7000 [ "a"; "b"; "c" ] in
  check "one instance per suite" (List.length inst = 3);
  let ports = List.map (fun i -> i.port) inst in
  check "suite 0 at base" (List.nth ports 0 = 7000);
  check "suite 1 at base+stride" (List.nth ports 1 = 7000 + stride);
  check "suite 2 at base+2*stride" (List.nth ports 2 = 7000 + (2 * stride));
  check "ports are distinct" (List.sort_uniq compare ports = List.sort compare ports);
  check "blocks don't overlap (stride > 1)" (stride > 1)

let%test_unit "allocate: env and fields" =
  let open Fennec_hunt_unit in
  let inst = allocate ~base:7000 [ "a"; "b"; "c" ] in
  let a = List.hd inst in
  check "url matches the port" (a.url = "http://localhost:7000");
  check "server_env sets the port" (List.assoc D.env_port a.server_env = "7000");
  check "server_env disables livereload (determinism)" (List.assoc D.env_dev_livereload a.server_env = "0");
  check "suite_env targets the instance via FENNEC_TEST_URL" (List.assoc D.env_test_url a.suite_env = "http://localhost:7000");
  check "suite name carried" (a.suite = "a")

let%test "deterministic (re-run gives identical ports)" =
  let ports1 = List.map (fun i -> i.port) (allocate ~base:7000 [ "a"; "b"; "c" ]) in
  let ports2 = List.map (fun i -> i.port) (allocate ~base:7000 [ "a"; "b"; "c" ]) in
  ports1 = ports2

let%test "a different base shifts the block" =
  (List.hd (allocate ~base:9000 [ "a" ])).port = 9000

let%test "empty suite list -> no instances" =
  allocate ~base:7000 [] = []

(* The load-bearing invariant, over ANY base and ANY suite list: one instance per suite, all ports
   distinct (no two suites ever collide), and every port at or above the base. This is what makes
   parallel runs safe — proven for the whole input space, not just the [a;b;c] example above. *)
let%prop "allocation is one-per-suite, distinct, and based at base" =
  let open Fennec_hunt_prop in
  forall
    ~print:(fun (base, names) -> Printf.sprintf "base=%d, %d suites" base (List.length names))
    Gen.(pair (int_range 1024 60000) (list_size (int_range 0 16) (string_size ~gen:char_printable (int_range 0 6))))
    (fun (base, names) ->
      let ports = List.map (fun (i : t) -> i.port) (allocate ~base names) in
      List.length ports = List.length names
      && List.sort_uniq compare ports = List.sort compare ports
      && List.for_all (fun p -> p >= base) ports)
