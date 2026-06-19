(* The "say hello" form model — ONE Sift codec shared by BOTH form stories: the server-rendered handler
   (frontend/handlers/hello.mlx, where [Form.read] coerces + validates the POST and [Form.input_attrs]
   stamps the <input>'s HTML5 constraints) AND the browser SPA component (frontend/components/hello_form.mlx,
   where [Form.signal] validates the same draft reactively, offline). The refinements live ONCE here as
   attributes; [@@deriving model] generates the codec + the typed [Fields] handles. A renamed field, or a
   tightened [@max_len], changes client validation, server validation, and the browser's native attrs
   together — there is no second copy to drift. Plain shared model; lives in the store so the server binary
   and the JS bundle agree byte-for-byte. *)

type t = { name : string [@trim] [@non_empty] [@max_len 40] }
[@@deriving model]

let empty = { name = "" }
