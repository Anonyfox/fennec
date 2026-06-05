(* A minimal, typed HTTP/1.1 client for testing — one request per connection, raw Eio sockets,
   zero external deps. Returns a structured response the assertion DSL pipes through.

   Not a general-purpose HTTP library: no keep-alive, no redirects, no chunked decoding, no TLS.
   Exactly what a test needs: send a request, read the full response, assert on it. *)

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

let header_value name resp =
  List.find_map (fun (k, v) -> if String.lowercase_ascii k = String.lowercase_ascii name then Some v else None) resp.headers

let request ~net ~port ~meth ~path ?(headers = []) ?body () : response =
  let body = Option.value body ~default:"" in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Net.with_tcp_connect ~host:"127.0.0.1" ~service:(string_of_int port) net @@ fun flow ->
  ignore addr;
  let has_host = List.exists (fun (k, _) -> String.lowercase_ascii k = "host") headers in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "%s %s HTTP/1.1\r\n" meth path);
  if not has_host then Buffer.add_string buf (Printf.sprintf "Host: localhost:%d\r\n" port);
  Buffer.add_string buf "Connection: close\r\n";
  List.iter (fun (k, v) -> Buffer.add_string buf (Printf.sprintf "%s: %s\r\n" k v)) headers;
  if body <> "" then Buffer.add_string buf (Printf.sprintf "Content-Length: %d\r\n" (String.length body));
  Buffer.add_string buf "\r\n";
  if body <> "" then Buffer.add_string buf body;
  Eio.Flow.copy_string (Buffer.contents buf) flow;
  Eio.Flow.shutdown flow `Send;
  (* read the full response *)
  let raw = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  let status_line = Eio.Buf_read.line raw in
  let status =
    match String.split_on_char ' ' status_line with
    | _ :: code :: _ -> ( match int_of_string_opt code with Some c -> c | None -> 0)
    | _ -> 0
  in
  let rec read_headers acc =
    let line = Eio.Buf_read.line raw in
    if line = "" || line = "\r" then List.rev acc
    else
      match String.index_opt line ':' with
      | Some i ->
        let k = String.trim (String.sub line 0 i) in
        let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
        read_headers ((k, v) :: acc)
      | None -> read_headers acc
  in
  let headers = read_headers [] in
  let body_buf = Buffer.create 4096 in
  (try
     while true do
       let chunk = Eio.Buf_read.any_char raw in
       Buffer.add_char body_buf chunk
     done
   with End_of_file -> ());
  { status; headers; body = Buffer.contents body_buf }

let get ~net ~port ?(headers = []) path = request ~net ~port ~meth:"GET" ~path ~headers ()
let post ~net ~port ?(headers = []) ?(body = "") path = request ~net ~port ~meth:"POST" ~path ~headers ~body ()
let put ~net ~port ?(headers = []) ?(body = "") path = request ~net ~port ~meth:"PUT" ~path ~headers ~body ()
let delete ~net ~port ?(headers = []) path = request ~net ~port ~meth:"DELETE" ~path ~headers ()
let head ~net ~port ?(headers = []) path = request ~net ~port ~meth:"HEAD" ~path ~headers ()
