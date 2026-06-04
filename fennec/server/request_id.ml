(* Request id — tag each request with a unique id (reusing an inbound one for trace
   propagation), exposed in a typed assign and echoed in a response header. Domain-SAFE:
   a one-time CSPRNG prefix + an Atomic counter, so ids stay unique across worker domains. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Assigns = Fennec_paw.Assigns

(* one CSPRNG prefix per process (hex of 4 random bytes), so ids from different runs/domains
   never collide; the Atomic counter makes them unique within the process *)
let prefix =
  let bytes =
    try
      let ic = open_in_bin "/dev/urandom" in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic 4)
    with _ -> "seed"
  in
  String.concat "" (List.init (String.length bytes) (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i])))

let counter = Atomic.make 0
let key : string Assigns.key = Assigns.key "fennec.request_id"

(* an inbound id is reused for trace propagation, but only if it's a sane token: non-empty,
   length-capped, and free of control bytes — so a client can't reflect a crafted value
   (header-injection / log-forging) through us into the response and logs. *)
let acceptable (v : string) : bool =
  v <> ""
  && String.length v <= 128
  && String.for_all (fun ch -> Char.code ch >= 0x20 && Char.code ch <> 0x7f) v

let make ?(header = "x-request-id") () : Paw.t =
 fun c ->
  let id =
    match Conn.req_header c header with
    | Some v when acceptable v -> v
    | _ -> Printf.sprintf "%s-%x" prefix (Atomic.fetch_and_add counter 1)
  in
  Conn.set_header (Conn.assign c key id) header id

(* the request id assigned by {!make}, if any *)
let current (c : Conn.t) : string option = Conn.get c key
