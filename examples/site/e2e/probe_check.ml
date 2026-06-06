(* I/O e2e proofs for the Http layer, against the pure-Eio probe_server (sibling binary):
   per-request timeout, redirect following, chunked decoding. Run manually. *)
open Fennec_hunt.Http

let probe = Filename.concat (Filename.dirname Sys.executable_name) "probe_server.exe"

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec at i j = j = ln || (i + j < lh && hay.[i + j] = needle.[j] && at i (j + 1)) in
  let rec scan i = i + ln <= lh && (at i 0 || scan (i + 1)) in
  ln = 0 || scan 0

let () = hunt "probe server" ~url:"http://localhost:4555" ~spawn:[ probe ] @@ fun () ->

  check "fast endpoint answers" (fun () ->
    get "/ok" ~expect:[status 200; body_is "ok"]);

  check "a hung request times out cleanly (not a frozen suite)" (fun () ->
    match get "/slow" ~timeout:0.3 with
    | () -> failwith "expected /slow to time out, but the request returned"
    | exception Failure m when contains m "timed out" -> ()  (* the desired outcome *)
    | exception Failure m -> failwith ("timed out for the wrong reason: " ^ m));

  check "eventually polls an async endpoint until it's done" (fun () ->
    (* /flaky is 503 for the first two hits, then 200 {"state":"done"} *)
    eventually ~within:5.0 ~interval:0.1 (fun () ->
        get "/flaky" ~expect:[status 200; json_path_is "state" "done"]));

  check "chunked response is decoded to its content" (fun () ->
    (* /chunked sends "ab"+"cd"+"ef" as three chunks; we assert the dechunked body *)
    get "/chunked" ~expect:[status 200; body_is "abcdef"]);

  check "multipart upload is sent intact (server echoes the body)" (fun () ->
    post "/echo"
      ~multipart:[ field "title" "hello"; file ~name:"doc" ~filename:"note.txt" ~content_type:"text/plain" "FILE-BYTES" ]
      ~expect:[
        status 200;
        body_contains {|name="title"|};
        body_contains "hello";
        body_contains {|name="doc"; filename="note.txt"|};
        body_contains "FILE-BYTES"])
