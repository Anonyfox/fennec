(* A server-rendered HANDLER — the middle layer between an inline Paw handler and a full SPA app.
   [get] renders an HTML page (a form) through the SSR engine, STATIC (no client JS); [post] reads
   typed input (autocast + the Codec model's validation), then redirects with a flash. Wired to
   /hello in server.ml. A real app would read/write Pulse here; this one just round-trips the flash.
   Authored as plain .ml with the Fur core builders ([h]/[text]/[attr], from [open Fennec.Fur]); the
   .mlx inline-HTML authoring of handlers is a follow-up DX step. *)
open Fennec.Fur

(* the form's typed input model — validation inline, the SAME Codec machinery as collections/methods *)
type t = { name : string [@trim] [@non_empty] [@max_len 40] }
[@@deriving model]

(* the page view — a Fur component tree, rendered to static HTML by Handler.html *)
let render conn ?flash ?(error = "") () =
  let p cls txt = h "p" [ class_ cls ] [ text txt ] in
  h "main" [ class_ "page" ]
    [ h "h1" [] [ text "Say hello" ];
      (match flash with Some m -> p "flash" m | None -> frag []);
      (if error = "" then frag [] else p "error" error);
      h "form" [ attr "method" "post"; attr "action" "/hello" ]
        [ Handler.csrf_field conn;
          h "input" [ attr "type" "text"; attr "name" "name"; attr "placeholder" "Your name" ] [];
          h "button" [ attr "type" "submit" ] [ text "Greet" ] ] ]

let get conn =
  let conn, flash = Handler.flash conn in
  Handler.html conn (render conn ?flash ())

let post conn =
  match Form.read codec conn with
  | Ok t -> Handler.redirect conn "/hello" ~flash:("Hello, " ^ t.name ^ "!")
  | Error _ -> Handler.html ~status:422 conn (render conn ~error:"Please enter a name (max 40 characters)." ())
