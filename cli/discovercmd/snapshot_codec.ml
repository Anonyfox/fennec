(* Compress / decompress the discover snapshot blob (zlib / DEFLATE).

   The snapshot is ~37 MB of marshaled API index — text-heavy, so it deflates to a fraction. It is
   embedded in the CLI binary, so compressing it shrinks the binary by ~30 MB. [gen_snapshot] compresses
   the marshaled bytes before emitting the generated [snapshot_data.ml]; that file's loader calls
   {!decompress} ONCE per `fennec discover` invocation, then [Marshal.from_string]s the result.

   One-shot whole-buffer codec over the [zlib] binding (the same library paw uses for HTTP gzip), with
   [Finish] so the deflate stream is complete + self-delimiting — no framing tricks needed. *)

let run (t : 'a Zlib.t) (input : string) : string =
  let n = String.length input in
  let inbuf = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout (max 1 n) in
  if n > 0 then Bigstringaf.blit_from_string input ~src_off:0 inbuf ~dst_off:0 ~len:n;
  t.Zlib.in_buf <- inbuf;
  t.Zlib.in_ofs <- 0;
  t.Zlib.in_len <- n;
  let cap = 0x10000 in
  let out = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout cap in
  let buf = Buffer.create (max 64 n) in
  (* Drive zlib to completion: a big blob produces output across MANY [flate] calls (each fills the
     64 KiB window), so we loop until [Stream_end] — or until a call makes no progress at all (neither
     consumes input nor emits output), the unambiguous "done / stuck" signal. The earlier
     in_len/out_len heuristic exited on the first partially-filled window and truncated the output. *)
  let rec loop () =
    t.Zlib.out_buf <- out;
    t.Zlib.out_ofs <- 0;
    t.Zlib.out_len <- cap;
    let before_in = t.Zlib.in_len in
    let st = Zlib.flate t Zlib.Finish in
    let produced = t.Zlib.out_ofs in
    if produced > 0 then Buffer.add_string buf (Bigstringaf.substring out ~off:0 ~len:produced);
    match st with
    | Zlib.Stream_end -> Buffer.contents buf
    | Zlib.Need_dict -> failwith "snapshot_codec: zlib needs a dictionary"
    | Zlib.Data_error m -> failwith ("snapshot_codec: zlib data error: " ^ m)
    | Zlib.Ok | Zlib.Buf_error ->
      if produced > 0 || t.Zlib.in_len < before_in then loop () else Buffer.contents buf
  in
  loop ()

(* deflate (best ratio — this runs at BUILD time, so the extra effort is free) *)
let compress (s : string) : string = run (Zlib.create_deflate ~level:9 ()) s

(* inflate (runs once at `fennec discover` startup) *)
let decompress (s : string) : string = run (Zlib.create_inflate ()) s

let roundtrips s = decompress (compress s) = s

let%test "round-trips the empty string" = roundtrips ""
let%test "round-trips short text" = roundtrips "hello, fennec"
let%test "round-trips binary with NULs + high bytes" = roundtrips "\x00\x01\xff\x80\x00\xfe a/b.mli"
let%test "round-trips a large mixed blob (covers multi-chunk output)" =
  let b = Buffer.create 200000 in
  for i = 0 to 49999 do Buffer.add_string b (Printf.sprintf "api:Mod%d.fn -> sig (%c)\n" i (Char.chr (33 + (i mod 90)))) done;
  roundtrips (Buffer.contents b)
let%test "a repetitive blob actually shrinks" = String.length (compress (String.make 50000 'x')) < 5000
