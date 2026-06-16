(* Drive Fennec_mail's SMTP client against a scripted SMTP server on a loopback socket — exercising the
   full MAIL_URL → boot → compose → SMTP submission path with no external mail daemon. Two scenarios:
   an unauthenticated server, and one advertising AUTH PLAIN (we assert the decoded credentials). *)

module Mail = Fennec_mail

let failf fmt = Printf.ksprintf (fun s -> prerr_endline ("mail/smtp FAIL: " ^ s); exit 1) fmt

let contains hay sub =
  let hl = String.length hay and sl = String.length sub in
  let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
  sl = 0 || go 0

let starts pfx s = String.length s >= String.length pfx && String.sub s 0 (String.length pfx) = pfx

type captured = {
  mutable mail_from : string option;
  mutable rcpts : string list;
  mutable data : string option;
  mutable auth : string option;
}

(* play the server side of one SMTP conversation over [flow], recording what the client sent *)
let serve flow ~advertise_auth ~(cap : captured) =
  let r = Eio.Buf_read.of_flow ~max_size:(4 * 1024 * 1024) flow in
  let say s = Eio.Flow.copy_string s flow in
  say "220 test.local ESMTP fennec-test\r\n";
  let rec loop () =
    match Eio.Buf_read.line r with
    | exception End_of_file -> ()
    | l ->
      let up = String.uppercase_ascii l in
      if starts "EHLO" up then (
        say (if advertise_auth then "250-test.local\r\n250 AUTH PLAIN LOGIN\r\n" else "250 test.local\r\n");
        loop ())
      else if starts "AUTH " up then (
        cap.auth <- Some (String.sub l 5 (String.length l - 5));
        say "235 2.7.0 authenticated\r\n";
        loop ())
      else if starts "MAIL FROM" up then (
        cap.mail_from <- Some l;
        say "250 2.1.0 ok\r\n";
        loop ())
      else if starts "RCPT TO" up then (
        cap.rcpts <- cap.rcpts @ [ l ];
        say "250 2.1.5 ok\r\n";
        loop ())
      else if up = "DATA" then (
        say "354 end data with <CRLF>.<CRLF>\r\n";
        let buf = Buffer.create 1024 in
        let rec read_data () =
          let dl = Eio.Buf_read.line r in
          if dl = "." then ()
          else (
            (* undo dot-stuffing so the captured bytes equal what was composed *)
            let dl = if String.length dl >= 1 && dl.[0] = '.' then String.sub dl 1 (String.length dl - 1) else dl in
            Buffer.add_string buf dl;
            Buffer.add_string buf "\r\n";
            read_data ())
        in
        read_data ();
        cap.data <- Some (Buffer.contents buf);
        say "250 2.0.0 queued\r\n";
        loop ())
      else if up = "QUIT" then say "221 2.0.0 bye\r\n"
      else (
        say "250 ok\r\n";
        loop ())
  in
  loop ()

let run ~net ~scenario ~auth ~advertise_auth =
  Eio.Switch.run @@ fun sw ->
  let port = 21000 + (Unix.getpid () mod 4000) + scenario in
  let socket = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let cap = { mail_from = None; rcpts = []; data = None; auth = None } in
  let result = ref (Error "client fiber did not run") in
  Eio.Fiber.both
    (fun () ->
      let flow, _addr = Eio.Net.accept ~sw socket in
      serve flow ~advertise_auth ~cap)
    (fun () ->
      let creds = match auth with Some (u, p) -> Printf.sprintf "%s:%s@" u p | None -> "" in
      Unix.putenv "MAIL_URL" (Printf.sprintf "smtp://%s127.0.0.1:%d" creds port);
      Mail.boot ~sw ~net ();
      let m =
        Mail.make
          ~from:(Mail.Address.v ~name:"Acme" "no-reply@acme.test")
          ~to_:[ Mail.Address.v "ada@example.com"; Mail.Address.v "bob@example.com" ]
          ~subject:"Verify" ~text:"click https://acme.test/v/abc" ~html:"<a>verify</a>" ()
      in
      result := (match Mail.send m with Ok () -> Ok () | Error e -> Error (Mail.string_of_error e)));
  (match !result with Ok () -> () | Error e -> failf "scenario %d: send failed: %s" scenario e);
  (match cap.mail_from with
  | Some s when contains s "<no-reply@acme.test>" -> ()
  | other -> failf "scenario %d: bad MAIL FROM: %s" scenario (Option.value other ~default:"<none>"));
  if List.length cap.rcpts <> 2 then failf "scenario %d: expected 2 RCPT TO, got %d" scenario (List.length cap.rcpts);
  (match cap.data with
  | Some d
    when contains d "Subject: Verify"
         && contains d "From: Acme <no-reply@acme.test>"
         && contains d "Content-Type: multipart/alternative;"
         && contains d (Base64.encode_string "click https://acme.test/v/abc") ->
    ()
  | _ -> failf "scenario %d: composed message not received intact" scenario);
  (* the AUTH PLAIN credential must decode to \0user\0pass *)
  (match (auth, cap.auth) with
  | Some (u, p), Some a when starts "PLAIN " a ->
    let decoded = Base64.decode_exn (String.sub a 6 (String.length a - 6)) in
    if decoded <> "\000" ^ u ^ "\000" ^ p then failf "scenario %d: AUTH PLAIN credential mismatch" scenario
  | Some _, _ -> failf "scenario %d: expected AUTH PLAIN, server saw %s" scenario (Option.value cap.auth ~default:"<none>")
  | None, Some _ -> failf "scenario %d: unexpected AUTH on an unauthenticated server" scenario
  | None, None -> ())

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let net = Eio.Stdenv.net env in
  run ~net ~scenario:0 ~auth:None ~advertise_auth:false;
  run ~net ~scenario:1 ~auth:(Some ("user", "s3cret")) ~advertise_auth:true;
  print_endline "mail/smtp: OK (MAIL_URL → boot → compose → submit; plaintext + AUTH PLAIN; DATA intact)"
