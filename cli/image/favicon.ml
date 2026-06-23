(* See favicon.mli. The set is data; generation is a fold over it through {!Engine.process}. *)

type icon = {
  filename : string;
  size : int;
  format : Format.t;
}

let set =
  [ { filename = "favicon.ico"; size = 32; format = Format.Ico };
    { filename = "apple-touch-icon.png"; size = 180; format = Format.Png };
    { filename = "icon-192.png"; size = 192; format = Format.Png };
    { filename = "icon-512.png"; size = 512; format = Format.Png } ]

let html_snippet =
  String.concat "\n"
    [ {|<link rel="icon" href="/favicon.ico" sizes="any">|};
      {|<link rel="apple-touch-icon" href="/apple-touch-icon.png">|};
      {|<link rel="manifest" href="/manifest.webmanifest">|} ]

let manifest_json =
  {|{
  "icons": [
    { "src": "/icon-192.png", "sizes": "192x192", "type": "image/png" },
    { "src": "/icon-512.png", "sizes": "512x512", "type": "image/png", "purpose": "any maskable" }
  ]
}
|}

let generate ~input ~dir =
  let ( let* ) = Result.bind in
  (* each icon is a centre-cropped square at its size *)
  let one (i : icon) =
    let op = { Op.default with resize = Some (Geometry.Box (i.size, i.size)); fit = Op.Cover } in
    let* bytes = Engine.process ~input ~format:i.format ~op in
    Engine.write_file (Filename.concat dir i.filename) bytes
  in
  let rec all = function
    | [] -> Ok ()
    | i :: rest ->
      let* () = one i in
      all rest
  in
  let* () = all set in
  let* () = Engine.write_file (Filename.concat dir "manifest.webmanifest") (Bytes.of_string manifest_json) in
  Ok html_snippet
