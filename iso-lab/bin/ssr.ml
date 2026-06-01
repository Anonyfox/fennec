let read f = In_channel.with_open_bin f In_channel.input_all

(* fake server "database" — stands in for real route handlers / publishers, called
   IN-PROCESS (no HTTP to ourselves, so no base-URL / relative-path problem). *)
let server_db = function
  | "/api/greeting" -> Some "Hello from the server \xf0\x9f\x91\x8b"
  | _ -> None  (* unknown / browser-only keys never resolve here *)

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let render = App.make () in
  let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  (* server SOURCE: don't fetch inline (would serialize the render) — record the key
     and let the driver run them concurrently between passes. Skip keys already
     resolved or already tried (so unresolved keys can't loop forever). *)
  Iso.Data.source := (fun key _k ->
    if not (Hashtbl.mem pending key) && not (Hashtbl.mem attempted key)
       && not (Hashtbl.mem Iso.Data.seed key)
    then Hashtbl.replace pending key ());
  let rec passes n =
    let html = Iso.to_html (render ()) in
    if Hashtbl.length pending = 0 || n > 16 then html
    else begin
      let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
      Hashtbl.clear pending;
      Eio.Fiber.all (List.map (fun key () ->
        Eio.Time.sleep clock 0.02;  (* simulate latency; the batch runs concurrently *)
        Hashtbl.replace attempted key ();
        match server_db key with Some v -> Iso.Data.put_seed key v | None -> ()) batch);
      passes (n + 1)
    end
  in
  let body = passes 0 in
  let client_js = read Sys.argv.(1) in
  let styles_dir = Sys.argv.(2) in
  let styles = Sys.readdir styles_dir |> Array.to_list |> List.sort compare
               |> List.map (fun f -> read (Filename.concat styles_dir f)) |> String.concat "\n" in
  let styles = "body{font-family:sans-serif}" ^ styles in
  let head = Iso.Head.to_ssr () in
  let data = Iso.Data.to_script () in
  let page = Iso.document (Document.make ~head ~data ~app:body ~styles ~client_js) in
  Out_channel.with_open_bin Sys.argv.(3) (fun oc -> Out_channel.output_string oc page)
