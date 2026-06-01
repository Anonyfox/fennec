let read f = In_channel.with_open_bin f In_channel.input_all
let () =
  let render = App.make () in            (* runs App setup -> registers default head *)
  let body = Iso.to_html (render ()) in  (* renders body -> child setups register head *)
  let head = Iso.Head.to_ssr () in       (* now resolve the collected, merged head *)
  let client_js = read Sys.argv.(1) in
  let styles_dir = Sys.argv.(2) in
  let styles = Sys.readdir styles_dir |> Array.to_list |> List.sort compare
               |> List.map (fun f -> read (Filename.concat styles_dir f)) |> String.concat "\n" in
  let styles = "body{font-family:sans-serif}" ^ styles in
  let page = Iso.document (Document.make ~head ~app:body ~styles ~client_js) in
  Out_channel.with_open_bin Sys.argv.(3) (fun oc -> Out_channel.output_string oc page)
