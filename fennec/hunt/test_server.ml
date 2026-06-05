(* Spawn and manage a server process for testing. Tied to an Eio switch: the server is killed
   when the switch ends, so cleanup is structural (no manual teardown). Supports --port for
   parallel isolated instances.

   The readiness probe is event-driven: Eio TCP connect in a tight retry loop with an Eio
   clock timeout — not a shell `sleep` poll. Typically ready in <100ms. *)

type t = {
  pid : int;
  port : int;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
}

let pid t = t.pid
let port t = t.port

let wait_ready ~net ~clock ~port ~timeout =
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if Eio.Time.now clock > deadline then failwith (Printf.sprintf "server on port %d never became ready (%.0fs timeout)" port timeout);
    match
      Eio.Net.with_tcp_connect ~host:"127.0.0.1" ~service:(string_of_int port) net (fun flow ->
          Eio.Flow.copy_string "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" flow;
          ignore (Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:4096)))
    with
    | () -> ()
    | exception _ -> Eio.Time.sleep clock 0.05; loop ()
  in
  loop ()

let spawn ~sw ~net ~clock ?(timeout = 30.0) ?(env = [||]) ~port (cmd : string list) : t =
  let argv = Array.of_list cmd in
  let full_env = Array.append (Unix.environment ()) env in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let pid = Unix.create_process_env argv.(0) argv full_env Unix.stdin devnull devnull in
  Unix.close devnull;
  Eio.Switch.on_release sw (fun () ->
      (try Unix.kill pid Sys.sigkill with _ -> ());
      (try ignore (Unix.waitpid [] pid) with _ -> ()));
  wait_ready ~net ~clock ~port ~timeout;
  { pid; port; net }

let signal t sig_ =
  (try Unix.kill t.pid sig_ with _ -> ());
  t

let port_free_within ~timeout port =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    let free =
      try
        let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
            Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
            false)
      with _ -> true
    in
    if free then ()
    else if Unix.gettimeofday () > deadline then failwith (Printf.sprintf "port %d still held after %.0fs" port timeout)
    else (Unix.sleepf 0.1; loop ())
  in
  loop ()

let port_held port =
  try
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
        true)
  with _ -> false

(* ---- convenience: make a request against this server ---- *)

let get t path = Http_client.get ~net:t.net ~port:t.port path
let get_h t ?(headers = []) path = Http_client.get ~net:t.net ~port:t.port ~headers path
let post t ?(body = "") path = Http_client.post ~net:t.net ~port:t.port ~body path
