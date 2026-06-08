(* Id generation. Meteor mints 17-char "unmistakable character" string ids by default; we also
   offer 24-hex ObjectIds. The randomness source is a parameter ([rng n] returns an int in [0,n))
   so tests can inject a deterministic stream and a JS build can plug in Math.random — the logic
   itself stays pure. *)

let unmistakable = "23456789ABCDEFGHJKLMNPQRSTWXYZabcdefghijkmnopqrstuvwxyz"
let hex_digits = "0123456789abcdef"

(* default randomness: Stdlib.Random works on native AND under js_of_ocaml *)
let default_rng n = Random.int n

let random_id ?(n = 17) ?(rng = default_rng) () =
  let len = String.length unmistakable in
  String.init n (fun _ -> unmistakable.[rng len])

let object_id ?(rng = default_rng) () =
  String.init 24 (fun _ -> hex_digits.[rng 16])
