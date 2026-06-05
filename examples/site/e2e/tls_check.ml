(* Proof that the Http layer speaks TLS — hunts against a self-signed HTTPS server over
   https://. Pure OCaml end to end (the server is tls_server.exe, a sibling binary). Run
   manually (like the other e2e binaries), not part of `dune test`. *)
open Fennec_hunt.Http

(* the self-signed HTTPS server built next to this binary *)
let tls_server = Filename.concat (Filename.dirname Sys.executable_name) "tls_server.exe"

let () = hunt "tls server (self-signed)" ~url:"https://localhost:8443" ~spawn:[ tls_server ] @@ fun () ->

  check "https GET works over TLS" (fun () ->
    get "/" ~expect:[
      status 200;
      is_json;
      json_path_is "secure" "true";
      json_path_is "via" "tls"]);

  check "https response timing is recorded" (fun () ->
    get "/";
    assert (elapsed_ms () >= 0.0))
