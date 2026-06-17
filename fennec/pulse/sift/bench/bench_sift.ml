(* Sift decode micro-benchmark — the zero-copy buffer path ({!Sift.decode_bytes}) vs the Bson.t tree
   path, apples-to-apples.

   Run:  dune exec fennec/pulse/sift/bench/bench_sift.exe

   Three numbers per fixture, all bytes → typed value:
     - tree parse+decode : Bson_wire.decode buf |> Sift.decode  — today's full end-to-end cost (the
                           storage layer parses the bytes into a Bson.t tree, then Sift walks it).
     - tree decode-only  : Sift.decode over an ALREADY-parsed Bson.t — the tree-walk ALONE, with the
                           parse pre-paid. The hard target: can a single pass from raw bytes beat just
                           the walk?
     - zerocopy bytes    : Sift.decode_bytes buf — one schema-directed pass, no tree.

   Reports ns/op (wall-clock) and B/op (bytes allocated — the GC-pressure metric that caps both
   single-op latency and OCaml-5 multicore throughput). Pure in-memory; numbers are stable run-to-run. *)

module B = Bson
module W = Mongo_wire.Bson_wire

let bench name ~iters (f : unit -> unit) =
  for _ = 1 to max 1 (iters / 10) do f () done;
  Gc.full_major ();
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iters do f () done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  let ns = (t1 -. t0) *. 1e9 /. float_of_int iters in
  let bytes = (a1 -. a0) /. float_of_int iters in
  Printf.printf "    %-22s %9.1f ns/op  %9.0f B/op\n%!" name ns bytes

let keep x = ignore (Sys.opaque_identity x)

(* run all three paths for one (codec, document) fixture *)
let fixture title ~iters (codec : 'a Sift.t) (doc : B.t) =
  let wire = W.encode doc in
  let buf = Bigstringaf.of_string ~off:0 ~len:(String.length wire) wire in
  let parsed = W.decode wire in
  (* sanity: all three agree before timing *)
  let r1 = Sift.decode codec (W.decode wire) and r2 = Sift.decode codec parsed and r3 = Sift.decode_bytes codec buf in
  assert (r1 = r2 && r2 = r3);
  Printf.printf "  %s  (%d wire bytes)\n%!" title (String.length wire);
  bench "tree parse+decode" ~iters (fun () -> keep (Sift.decode codec (W.decode wire)));
  bench "tree decode-only" ~iters (fun () -> keep (Sift.decode codec parsed));
  bench "zerocopy bytes" ~iters (fun () -> keep (Sift.decode_bytes codec buf))

(* ── fixtures ─────────────────────────────────────────────────────────────── *)

(* 1. a small, typical record — 4 fields, all wanted (the 90% case) *)
let small_codec =
  Sift.(
    seal
      (record (fun id title count active -> (id, title, count, active))
      |> field (req "_id" id) (fun (a, _, _, _) -> a)
      |> field (req "title" string) (fun (_, a, _, _) -> a)
      |> field (req "count" int) (fun (_, _, a, _) -> a)
      |> field (req "active" bool) (fun (_, _, _, a) -> a)))

let small_doc =
  B.doc [ ("_id", B.oid "507f1f77bcf86cd799439011"); ("title", B.str "Buy milk"); ("count", B.int 3); ("active", B.bool true) ]

(* 2. a WIDE document (20 mixed fields) but a NARROW shape (wants 3) — the schema-directed win:
      decode_bytes skips 17 fields by their length prefix; the tree path parses all 20 *)
let wide_codec =
  Sift.(
    seal
      (record (fun a b c -> (a, b, c))
      |> field (req "f00" string) (fun (a, _, _) -> a)
      |> field (req "f10" int) (fun (_, b, _) -> b)
      |> field (req "f19" bool) (fun (_, _, c) -> c)))

let wide_doc =
  B.doc
    (List.init 20 (fun i ->
         let k = Printf.sprintf "f%02d" i in
         let v =
           match i mod 5 with
           | 0 -> B.str (Printf.sprintf "value-%d" i)
           | 1 -> B.int (i * 7)
           | 2 -> B.Float (float_of_int i +. 0.5)
           | 3 -> B.bool (i land 1 = 0)
           | _ -> B.doc [ ("nested", B.int i); ("label", B.str "x") ]
         in
         (k, v)))

(* 3. a NESTED record — sub-document + a small list (the realistic blog-post shape) *)
let nested_codec =
  let author = Sift.(seal (record (fun n e -> (n, e)) |> field (req "name" string) fst |> field (req "email" string) snd)) in
  Sift.(
    seal
      (record (fun title author tags -> (title, author, tags))
      |> field (req "title" string) (fun (a, _, _) -> a)
      |> field (req "author" author) (fun (_, a, _) -> a)
      |> field (opt_list "tags" string) (fun (_, _, a) -> a)))

let nested_doc =
  B.doc
    [ ("title", B.str "A post about zero-copy decoding");
      ("author", B.doc [ ("name", B.str "Ada Lovelace"); ("email", B.str "ada@analytical.engine") ]);
      ("tags", B.array [ B.str "ocaml"; B.str "bson"; B.str "performance"; B.str "sift" ]) ]

(* 4. a LIST-heavy document — an array of 100 ints (bulk numeric payload) *)
let list_codec = Sift.(seal (record (fun xs -> xs) |> field (req "xs" (list int)) (fun xs -> xs)))
let list_doc = B.doc [ ("xs", B.array (List.init 100 (fun i -> B.int i))) ]

(* 5. a list of 50 small RECORDS (array of sub-documents — the heaviest tree to build) *)
let rows_codec =
  let row = Sift.(seal (record (fun id n -> (id, n)) |> field (req "id" int) fst |> field (req "n" string) snd)) in
  Sift.(seal (record (fun rows -> rows) |> field (req "rows" (list row)) (fun rows -> rows)))

let rows_doc =
  B.doc [ ("rows", B.array (List.init 50 (fun i -> B.doc [ ("id", B.int i); ("n", B.str (Printf.sprintf "row-%d" i)) ]))) ]

let () =
  Printf.printf "\nSift decode: zero-copy buffer path vs Bson.t tree path (bytes -> typed value)\n\n%!";
  fixture "1. small record (4 fields, all wanted)" ~iters:2_000_000 small_codec small_doc;
  fixture "2. wide doc (20 fields), narrow shape (wants 3)" ~iters:1_000_000 wide_codec wide_doc;
  fixture "3. nested record (sub-doc + list)" ~iters:1_000_000 nested_codec nested_doc;
  fixture "4. list of 100 ints" ~iters:500_000 list_codec list_doc;
  fixture "5. list of 50 sub-records" ~iters:200_000 rows_codec rows_doc;
  Printf.printf "\n%!"
