(* Unit tests for multipart/form-data parsing. *)

module M = Fennec_core.Multipart

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let body =
  String.concat ""
    [ "--X\r\n";
      "Content-Disposition: form-data; name=\"title\"\r\n\r\n";
      "Hello World\r\n";
      "--X\r\n";
      "Content-Disposition: form-data; name=\"upload\"; filename=\"a.txt\"\r\n";
      "Content-Type: text/plain\r\n\r\n";
      "file\r\ncontents\r\n";
      "--X--\r\n" ]

let () =
  print_endline "Multipart.boundary_of_content_type:";
  eq "extracts boundary" (M.boundary_of_content_type "multipart/form-data; boundary=X") (Some "X");
  eq "quoted boundary" (M.boundary_of_content_type {|multipart/form-data; boundary="a b"|}) (Some "a b");
  eq "absent boundary" (M.boundary_of_content_type "text/plain") None;

  print_endline "Multipart.parse:";
  let parts = M.parse ~boundary:"X" body in
  eq "two parts" (List.length parts) 2;
  (match parts with
   | [ field; file ] ->
     eq "field name" field.M.name "title";
     eq "field has no filename" field.M.filename None;
     eq "field data" field.M.data "Hello World";
     eq "file name" file.M.name "upload";
     eq "file filename" file.M.filename (Some "a.txt");
     eq "file content-type" file.M.content_type "text/plain";
     eq "file data (inner CRLF kept)" file.M.data "file\r\ncontents"
   | _ -> check "expected exactly two parts" false);
  eq "empty body -> no parts" (M.parse ~boundary:"X" "") [];

  if !fails = 0 then print_endline "all Multipart tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
