(* SockJS framing for the websocket transport — the thin compat layer so a stock Meteor browser
   client (which dials /sockjs first) can speak to us. Raw /websocket is the primary path; this only
   wraps/unwraps DDP JSON.

   Server->client frames: 'o' (open), 'h' (heartbeat), a["<ddp>",…] (messages), c[code,"reason"]
   (close). Client->server: ["<ddp>",…] arrays or a bare "<ddp>" string. Pure -> native + JS. *)

let open_frame = "o"
let heartbeat = "h"

(* server -> client: a["<ddp1>","<ddp2>"] (each DDP message JSON-string-escaped) *)
let wrap (msgs : string list) : string =
  "a" ^ Json.to_string (Json.List (List.map (fun s -> Json.String s) msgs))

(* client -> server: ["<ddp1>","<ddp2>"] (a JSON array of message strings, no 'a' prefix) — what
   our SockJS *client* sends to dial a Meteor server *)
let client_frame (msgs : string list) : string =
  Json.to_string (Json.List (List.map (fun s -> Json.String s) msgs))

(* is this frame a SockJS array-of-messages frame (server 'a[...]' or client '[...]')? control
   frames ('o','h','c[...]') are handled separately *)
let is_array_frame (frame : string) : bool =
  String.length frame > 0 && (frame.[0] = 'a' || frame.[0] = '[')

let close_frame code reason =
  "c" ^ Json.to_string (Json.List [ Json.Number (float_of_int code); Json.String reason ])

(* client -> server: ["<ddp>",…] | "<ddp>" | a[…] -> the DDP JSON strings *)
let unwrap (frame : string) : string list =
  if frame = "" then []
  else
    let body =
      if frame.[0] = 'a' && String.length frame > 1 then String.sub frame 1 (String.length frame - 1)
      else frame
    in
    match (try Some (Json.parse body) with _ -> None) with
    | Some (Json.List xs) -> List.filter_map (function Json.String s -> Some s | _ -> None) xs
    | Some (Json.String s) -> [ s ]
    | _ -> []

(* the /sockjs/info handshake payload (entropy supplied by the caller) *)
let info ~entropy : string =
  Json.to_string
    (Json.Obj
       [ ("websocket", Json.Bool true);
         ("origins", Json.List [ Json.String "*:*" ]);
         ("cookie_needed", Json.Bool false);
         ("entropy", Json.Number (float_of_int entropy)) ])

(* a SockJS websocket path is /sockjs/<server>/<session>/websocket *)
let is_sockjs_path (path : string) : bool =
  let contains hay needle =
    let nh = String.length hay and nn = String.length needle in
    let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
    nn = 0 || go 0
  in
  contains path "/sockjs/"
