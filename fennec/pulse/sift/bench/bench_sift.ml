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
module Ffi = Fennec_mongo_ffi.Mongo_ffi

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

(* A pure-OCaml structural walk of a BSON buffer — the OCaml analog of the libbson C walk: iterate every
   field, touch its key + value first byte, recurse into sub-docs/arrays, skip scalars by their length
   prefix. Helpers are TOP-LEVEL (buf passed explicitly, no captured closure per call) so it is genuinely
   zero-allocation, unsafe reads on the known-good fixture (mirrors the tuned Cursor's post-bounds-check
   codegen). This isolates OCaml SCAN speed from value MATERIALISATION: the gap to [zerocopy bytes] is the
   cost of building owned OCaml values; the gap to [libbson walk (C)] is pure OCaml-vs-C scanning. *)
let wu8 buf at = Char.code (Bigstringaf.unsafe_get buf at)
let wi32 buf at = Int32.to_int (Bigstringaf.unsafe_get_int32_le buf at)
let rec wnul buf at = if wu8 buf at = 0 then at else wnul buf (at + 1)

let wvsize buf at tag =
  match tag with
  | 0x01 | 0x09 | 0x11 | 0x12 -> 8
  | 0x02 | 0x0d | 0x0e -> 4 + wi32 buf at
  | 0x03 | 0x04 | 0x0f -> wi32 buf at
  | 0x05 -> 5 + wi32 buf at
  | 0x07 -> 12
  | 0x08 -> 1
  | 0x0a | 0xff | 0x7f -> 0
  | 0x10 -> 4
  | 0x13 -> 16
  | 0x0b -> wnul buf (wnul buf at + 1) + 1 - at
  | _ -> 0

let rec wwalk buf pos acc = wloop buf (pos + 4) acc
and wloop buf p acc =
  let tag = wu8 buf p in
  if tag = 0 then acc
  else
    let kstart = p + 1 in
    let voff = wnul buf kstart + 1 in
    let acc = acc + wu8 buf kstart in
    if tag = 0x03 || tag = 0x04 then wloop buf (voff + wi32 buf voff) (wwalk buf voff acc)
    else
      let vsz = wvsize buf voff tag in
      let acc = if vsz > 0 then acc + wu8 buf voff else acc in
      wloop buf (voff + vsz) acc

let ocaml_walk (buf : Bigstringaf.t) : int = wwalk buf 0 0

(* The STAGING CEILING probe: a hand-written direct decoder for the small fixture — scans for each field
   and applies the constructor in ONE call (id, title, count, active), with NO applicative builder (no
   incremental currying of an N-ary make, no Ok threading, no per-field/closure plumbing). It still
   materialises the same owned OCaml values (the oid hex string, the title copy) as decode_bytes, so the
   gap between this and [zerocopy bytes] is precisely the applicative-builder tax — what a ppx/staged
   decoder (K4) would recover. Pure OCaml, no Obj.magic. *)
let hexc = "0123456789abcdef"
let woid buf at = String.init 24 (fun i -> let b = wu8 buf (at + (i / 2)) in if i land 1 = 0 then hexc.[b lsr 4] else hexc.[b land 0xf])
let wstr buf at = let len = wi32 buf at in Bigstringaf.substring buf ~off:(at + 4) ~len:(len - 1)

(* find a top-level field by key; returns its value tag (or 0 if absent), leaving the value offset in
   [vref]. Mirrors scan_find: rescans from the doc start, byte-compares keys in place. Helpers are
   top-level (no per-call closure) so the probe measures the true staged ceiling, not bench artifacts. *)
let rec wkeyeq buf kstart key klen i = i >= klen || (wu8 buf (kstart + i) = Char.code (String.unsafe_get key i) && wkeyeq buf kstart key klen (i + 1))
let rec wfind_loop buf key klen vref p =
  let tag = wu8 buf p in
  if tag = 0 then 0
  else
    let kstart = p + 1 in
    let kend = wnul buf kstart in
    let voff = kend + 1 in
    if kend - kstart = klen && wkeyeq buf kstart key klen 0 then (vref := voff; tag) else wfind_loop buf key klen vref (voff + wvsize buf voff tag)
let wfind buf key vref = wfind_loop buf key (String.length key) vref 4

let hand_decode_small buf : (string * string * int * bool) option =
  let v = ref 0 in
  let t_id = wfind buf "_id" v in let o_id = !v in
  let t_ti = wfind buf "title" v in let o_ti = !v in
  let t_co = wfind buf "count" v in let o_co = !v in
  let t_ac = wfind buf "active" v in let o_ac = !v in
  if t_id = 0 || t_ti = 0 || t_co = 0 || t_ac = 0 then None
  else
    let id = if t_id = 0x07 then woid buf o_id else wstr buf o_id in
    Some (id, wstr buf o_ti, wi32 buf o_co, wu8 buf o_ac <> 0)

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
  bench "zerocopy bytes" ~iters (fun () -> keep (Sift.decode_bytes codec buf));
  bench "ocaml scan-only" ~iters (fun () -> keep (ocaml_walk buf));
  (* the "official" C reference: libbson iterates the SAME buffer in place (borrowed, no tree, no OCaml
     values). A full walk — so for the wide/narrow fixture it reads all 20 fields where decode_bytes
     skips 17; for the all-fields-wanted fixtures it is the honest parse-speed floor. *)
  if Ffi.available () then bench "libbson walk (C)" ~iters (fun () -> keep (Ffi.bson_bench_walk buf));
  (* T1 alloc-free tier: validate WITHOUT materializing — should run at scan speed, ~0 alloc, beating C. *)
  bench "valid_bytes (T1)" ~iters (fun () -> keep (Sift.valid_bytes codec buf));
  bench "scan_valid (T1)" ~iters (fun () -> keep (Sift.scan_valid buf));
  (* K3 encode: zero-copy (size→alloc→write) vs today's tree path (enc → Bson.t → Bson_wire.encode) *)
  match Sift.decode codec parsed with
  | Error _ -> ()
  | Ok value ->
      bench "tree encode" ~iters (fun () -> keep (W.encode (codec.Sift.enc value)));
      bench "zerocopy encode" ~iters (fun () -> keep (Sift.encode_bytes codec value));
      bench "size only" ~iters (fun () -> keep (Sift.size codec value))

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
         (* the three WANTED fields get their shape's type; the other 17 are mixed junk to be skipped *)
         let v =
           if k = "f00" then B.str "value-0"
           else if k = "f10" then B.int 70
           else if k = "f19" then B.bool true
           else
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
  Printf.printf "\n  staging ceiling probe (small record): hand-written direct decode vs the applicative builder\n%!";
  let swire = W.encode small_doc in
  let sbuf = Bigstringaf.of_string ~off:0 ~len:(String.length swire) swire in
  (match (Sift.decode_bytes small_codec sbuf, hand_decode_small sbuf) with
  | Ok v, Some v' -> assert (v = v') (* the hand decoder agrees with decode_bytes *)
  | _ -> assert false);
  bench "zerocopy bytes" ~iters:2_000_000 (fun () -> keep (Sift.decode_bytes small_codec sbuf));
  bench "hand direct decode" ~iters:2_000_000 (fun () -> keep (hand_decode_small sbuf));
  Printf.printf "\n%!"
