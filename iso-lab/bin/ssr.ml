let read f = In_channel.with_open_bin f In_channel.input_all

(* the app's in-process data source (path -> json), called directly — no HTTP-to-self *)
let source = function "/api/greeting" -> Some "Hello from the server \xf0\x9f\x91\x8b" | _ -> None

(* the mount table: which apps live at which base. The server knows them all. *)
let mounts = [ { Iso_ssr.base = "/shop"; root = App.make; router = Routes.router } ]

let () =
  Eio_main.run @@ fun env ->
  let request = if Array.length Sys.argv > 4 then Sys.argv.(4) else "/shop/products/7" in
  let client_js = read Sys.argv.(1) in
  let styles_dir = Sys.argv.(2) in
  let styles = Sys.readdir styles_dir |> Array.to_list |> List.sort compare
               |> List.map (fun f -> read (Filename.concat styles_dir f)) |> String.concat "\n" in
  let styles = "body{font-family:sans-serif}" ^ styles in
  match Iso_ssr.render ~env ~mounts ~source ~document:Document.make ~request ~client_js ~styles () with
  | Some page -> Out_channel.with_open_bin Sys.argv.(3) (fun oc -> Out_channel.output_string oc page)
  | None -> failwith "no app mounted for request"
