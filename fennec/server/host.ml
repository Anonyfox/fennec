(* Host-header pattern matching (pure) — how the server routes a request to one of
   several endpoints sharing a port in production. Patterns:

     "example.com"        exact
     "*.example.com"      one or more leading labels (api.example.com, a.b.example.com)
     "*"                  any host (catch-all)

   The Host header's port suffix (":8200") is ignored. Matching is
   case-insensitive. *)

(* strip a ":port" suffix and lowercase *)
let normalize (host : string) : string =
  let host = match String.index_opt host ':' with Some i -> String.sub host 0 i | None -> host in
  String.lowercase_ascii (String.trim host)

let matches ~(pattern : string) (host : string) : bool =
  let host = normalize host in
  let pattern = String.lowercase_ascii (String.trim pattern) in
  if pattern = "*" then true
  else if String.length pattern > 2 && String.sub pattern 0 2 = "*." then
    (* suffix match on ".rest", requiring at least one leading label *)
    let suffix = String.sub pattern 1 (String.length pattern - 1) (* ".example.com" *) in
    let slen = String.length suffix and hlen = String.length host in
    hlen > slen && String.sub host (hlen - slen) slen = suffix
  else pattern = host
