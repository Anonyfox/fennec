(* Server render driver. Dispatches a request to a mounted app (longest base wins,
   strips the base), runs its data fetches concurrently between render passes (Eio),
   then assembles the app's chosen document. Generic over the generated [mount list].

   IMPORTANT: per-request isolation — the module globals touched here (Router active/
   current, Data.seed + source, Head.sources) must become Eio fiber-local before this
   runs concurrently on the real server; [render]'s body is that unit of work. *)
let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dispatch (mounts : Fur.mount list) path =
  List.filter (fun (m : Fur.mount) -> m.base = "" || path = m.base || starts_with (m.base ^ "/") path) mounts
  |> List.sort (fun (a : Fur.mount) b -> compare (String.length b.base) (String.length a.base))
  |> function m :: _ -> Some m | [] -> None

let render ~env ~(mounts : Fur.mount list) ~source ~request ~client_js ~styles () : string option =
  match dispatch mounts request with
  | None -> None
  | Some m ->
    let clock = Eio.Stdenv.clock env in
    Fur.Router.activate m.router;
    Fur.Router.set_path m.router request;
    let render_root = m.root () in
    let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    Fur.Data.source := (fun key _ ->
      if not (Hashtbl.mem pending key) && not (Hashtbl.mem attempted key)
         && not (Hashtbl.mem Fur.Data.seed key) then Hashtbl.replace pending key ());
    let rec passes n =
      let html = Fur.to_html (render_root ()) in
      if Hashtbl.length pending = 0 || n > 16 then html
      else begin
        let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
        Hashtbl.clear pending;
        Eio.Fiber.all (List.map (fun key () ->
          Eio.Time.sleep clock 0.02;
          Hashtbl.replace attempted key ();
          match source key with Some v -> Fur.Data.put_seed key v | None -> ()) batch);
        passes (n + 1)
      end
    in
    let body = passes 0 in
    let ctx = { Fur.Doc.head = Fur.Head.to_ssr (); data = Fur.Data.to_script (); body; styles; client_js } in
    Some (Fur.document (m.document ctx))
