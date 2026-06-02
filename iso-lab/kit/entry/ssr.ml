let read f = In_channel.with_open_bin f In_channel.input_all
let () =
  Eio_main.run @@ fun env ->
  let request = if Array.length Sys.argv > 4 then Sys.argv.(4) else "/shop/products/7" in
  let client_js = read Sys.argv.(1) in
  let styles_dir = Sys.argv.(2) in
  let styles = (try Sys.readdir styles_dir |> Array.to_list with _ -> []) |> List.sort compare
               |> List.map (fun f -> read (Filename.concat styles_dir f)) |> String.concat "\n" in
  let styles = "body{font-family:sans-serif}" ^ styles in
  match Iso_ssr.render ~env ~mounts:Routes_gen.apps ~source:Api.source ~request ~client_js ~styles () with
  | Some page -> Out_channel.with_open_bin Sys.argv.(3) (fun oc -> Out_channel.output_string oc page)
  | None -> failwith "no app mounted for request"
