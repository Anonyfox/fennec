(* Unit tests for the HTTP request parser: Content-Length bodies, chunked decoding,
   the Content-Length+Transfer-Encoding smuggling rejection, and Expect: 100-continue. *)

module S = Fennec_server.Server

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let parse ?(continue = fun () -> ()) s = S.read_request ~continue (Eio.Buf_read.of_string s)
let body_of = function S.Req p -> Some p.S.body | _ -> None
let is_bad = function S.Bad_request _ -> true | _ -> false

let () =
  print_endline "Server.read_request:";
  (* GET, no body *)
  eq "GET no body" (body_of (parse "GET / HTTP/1.1\r\nHost: a\r\n\r\n")) (Some "");
  (* Content-Length body *)
  eq "content-length body"
    (body_of (parse "POST /x HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nHello"))
    (Some "Hello");
  (* chunked body, two chunks *)
  eq "chunked body decoded"
    (body_of
       (parse
          "POST /x HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n \
           World\r\n0\r\n\r\n"))
    (Some "Hello World");
  (* chunked with a chunk extension on the size line *)
  eq "chunk extension tolerated"
    (body_of
       (parse "POST /x HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n3;x=1\r\nabc\r\n0\r\n\r\n"))
    (Some "abc");
  (* Content-Length + Transfer-Encoding together -> rejected (smuggling) *)
  check "CL + TE rejected as smuggling"
    (is_bad
       (parse
          "POST /x HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\nTransfer-Encoding: chunked\r\n\r\nHello"));
  (* Expect: 100-continue invokes the continue callback before the body *)
  let called = ref false in
  let _ =
    parse ~continue:(fun () -> called := true)
      "POST /x HTTP/1.1\r\nHost: a\r\nExpect: 100-continue\r\nContent-Length: 2\r\n\r\nhi"
  in
  check "Expect: 100-continue triggers the callback" !called;
  (* no Expect -> callback not called *)
  let called2 = ref false in
  let _ = parse ~continue:(fun () -> called2 := true) "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  check "no Expect: callback not called" (not !called2);
  (* malformed request line *)
  check "malformed request line rejected" (is_bad (parse "GARBAGE\r\n\r\n"));

module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let status_of c = match Conn.resp c with Some r -> r.H.status | None -> 0

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let req = H.make_request ~meth:H.GET ~path:"/" () in
  print_endline "Server.run_handler (per-request deadline + cancellation):";
  (* a normal handler answers as usual *)
  eq "normal handler -> 200" (status_of (S.run_handler ~clock ~on_error:S.default_on_error ~timeout:1.0 (fun c -> Conn.text c "ok") req)) 200;
  (* a handler that hangs is cancelled at the deadline -> 503 *)
  eq "hung handler -> 503"
    (status_of (S.run_handler ~clock ~on_error:S.default_on_error ~timeout:0.05 (fun _ -> Eio.Fiber.await_cancel ()) req)) 503;
  (* a throwing handler -> 500 *)
  eq "throwing handler -> 500"
    (status_of (S.run_handler ~clock ~on_error:S.default_on_error ~timeout:1.0 (fun _ -> failwith "boom") req)) 500;
  (* cancellation PROPAGATES to forked sub-fibers: a handler forking two 10s sleeps is
     cancelled in full and returns at the deadline, not 10s later *)
  let t0 = Eio.Time.now clock in
  let c =
    S.run_handler ~clock ~on_error:S.default_on_error ~timeout:0.05
      (fun c -> Eio.Fiber.both (fun () -> Eio.Time.sleep clock 10.0) (fun () -> Eio.Time.sleep clock 10.0); c)
      req
  in
  check "sub-fibers cancelled (returns at the deadline, not after 10s)" (Eio.Time.now clock -. t0 < 1.0);
  eq "cancelled handler -> 503" (status_of c) 503;

  print_endline "Fennec.parallel / both (structured concurrency):";
  let t0 = Eio.Time.now clock in
  let rs = Fennec.parallel [ (fun () -> Eio.Time.sleep clock 0.1; 1); (fun () -> Eio.Time.sleep clock 0.1; 2) ] in
  let elapsed = Eio.Time.now clock -. t0 in
  eq "parallel returns results in order" rs [ 1; 2 ];
  check "parallel overlaps the waits (~0.1s, not 0.2s)" (elapsed < 0.18);
  let a, b = Fennec.both (fun () -> Eio.Time.sleep clock 0.1; "x") (fun () -> Eio.Time.sleep clock 0.1; 7) in
  check "both returns a typed pair" (a = "x" && b = 7)

let () =
  if !fails = 0 then print_endline "all Server tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
