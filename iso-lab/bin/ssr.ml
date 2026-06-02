let read f = In_channel.with_open_bin f In_channel.input_all
let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* fake server "database" — in-process route handlers (no HTTP-to-self). *)
let server_db = function "/api/greeting" -> Some "Hello from the server \xf0\x9f\x91\x8b" | _ -> None

(* The MOUNT TABLE: (base, app-shell, router). The server knows every app
   precisely; dispatch strips the longest matching base and renders that app, so
   apps are location-transparent. (One app here; adding more = more rows + bundles.) *)
let mounts = [ ("/shop", App.make, Routes.router) ]
let dispatch path =
  List.filter (fun (base, _, _) -> base = "" || path = base || starts_with (base ^ "/") path) mounts
  |> List.sort (fun (a, _, _) (b, _, _) -> compare (String.length b) (String.length a))
  |> function (_, make, router) :: _ -> Some (make, router) | [] -> None

(* IMPORTANT: per-request isolation. This binary renders ONE request per process, so
   mutating the module globals (Iso.Router.current via set_path, Iso.Data.seed via the
   driver, Iso.Head.sources during render) is safe here. fennec's REAL server is
   concurrent (Eio fibers); wrap each request's render in fiber-local bindings for
   those globals (Eio.Fiber.with_binding over a per-request context) so simultaneous
   requests don't share head/data/route state. The two-pass driver below is the unit
   of work that must run inside that per-request scope. *)
let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let request_path = if Array.length Sys.argv > 4 then Sys.argv.(4) else "/shop/products/7" in
  let make, router =
    match dispatch request_path with Some x -> x | None -> failwith "no app mounted for path" in
  Iso.Router.set_path router request_path;   (* strip base -> relative path *)
  let render = make () in
  let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  Iso.Data.source := (fun key _ ->
    if not (Hashtbl.mem pending key) && not (Hashtbl.mem attempted key)
       && not (Hashtbl.mem Iso.Data.seed key) then Hashtbl.replace pending key ());
  let rec passes n =
    let html = Iso.to_html (render ()) in
    if Hashtbl.length pending = 0 || n > 16 then html
    else begin
      let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
      Hashtbl.clear pending;
      Eio.Fiber.all (List.map (fun key () ->
        Eio.Time.sleep clock 0.02;
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
