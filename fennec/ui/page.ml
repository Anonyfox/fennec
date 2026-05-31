(* Server-side page rendering — the SSR half of the isomorphic loop.

   The framework owns the boilerplate of turning a rendered component into a full
   HTML document: the <head>, the SSR'd markup inside #root, the props JSON the
   client reads to hydrate, and the asset <link>/<script> tags. The app just hands
   over an element + props; everything below stays here so userland code is lean.

   No mongo, no reactive store: hydration data is plain JSON props the app chooses.
   The client reads <script id="fennec-props"> and mounts the SAME component with
   those props, so the first client paint matches the SSR markup exactly. *)

(* Escape a string for safe embedding inside an HTML element / <script> JSON.
   For the inline JSON we only need to neutralize </script> and the HTML specials
   that could break out of the text context. *)
let escape_json_for_script (s : string) : string =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      match c with
      | '<' -> Buffer.add_string b "\\u003c"
      | '>' -> Buffer.add_string b "\\u003e"
      | '&' -> Buffer.add_string b "\\u0026"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let attr s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "&quot;"
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Build the full HTML document.

   @param title     document <title>.
   @param description optional meta description.
   @param head      extra raw markup injected into <head> (e.g. preload hints).
   @param css_href  stylesheet URL (a <link> is emitted when given).
   @param scripts   script URLs, emitted as <script defer> in order. The client
                    bundle goes last by convention.
   @param props_json JSON string inlined as <script id="fennec-props">, read by
                    the client to hydrate. Defaults to "null".
   @param body_html the SSR-rendered component markup (goes inside #root).
   @param dev       when true, the livereload script is injected (see Dev). *)
let document ?(title = "") ?(description = "") ?(head = "") ?css_href ?(scripts = [])
    ?(props_json = "null") ?(dev = false) ~body_html () : string =
  let link =
    match css_href with
    | Some href -> Printf.sprintf {|<link rel="stylesheet" href="%s"/>|} (attr href)
    | None -> ""
  in
  let desc =
    if description = "" then ""
    else Printf.sprintf {|<meta name="description" content="%s"/>|} (attr description)
  in
  let script_tags =
    String.concat "\n"
      (List.map (fun s -> Printf.sprintf {|<script src="%s" defer></script>|} (attr s)) scripts)
  in
  let doc =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>%s</title>
%s
%s
%s
</head>
<body>
<div id="root">%s</div>
<script id="fennec-props" type="application/json">%s</script>
%s
</body>
</html>|}
      (attr title) desc link head body_html
      (escape_json_for_script props_json)
      script_tags
  in
  if dev then Fennec_core.Dev.inject_html doc else doc
