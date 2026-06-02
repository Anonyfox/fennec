(* Server render driver — the whole SSR pipeline behind one call.
   [render] dispatches a request to a mounted app (longest base prefix wins, strips
   the base), runs its data fetches concurrently between render passes (Eio), then
   assembles the document. The app's server entry just declares the mount table, the
   data source, and the document shell.

   IMPORTANT: per-request isolation. This renders ONE request; the module globals it
   touches (Router.current/cur_params + active, Data.seed + source, Head.sources)
   must become Eio fiber-local before this runs concurrently on the real server. The
   body of [render] is exactly the unit of work to wrap in a per-request scope. *)

type mount = { base : string; root : unit -> unit -> Iso.vnode; router : Iso.Router.t }

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

let dispatch mounts path =
  List.filter (fun m -> m.base = "" || path = m.base || starts_with (m.base ^ "/") path) mounts
  |> List.sort (fun a b -> compare (String.length b.base) (String.length a.base))
  |> function m :: _ -> Some m | [] -> None

(* [source]: the app's in-process data fn (path -> json option). [render] returns the
   full HTML document string, or None if no app is mounted for [request]. *)
let render ~env ~(mounts : mount list) ~(source : string -> string option)
    ~(document : Iso.Doc.ctx -> Iso.vnode) ~request ~client_js ~styles () : string option =
  match dispatch mounts request with
  | None -> None
  | Some m ->
    let clock = Eio.Stdenv.clock env in
    Iso.Router.activate m.router;
    Iso.Router.set_path m.router request;       (* strip base -> relative path *)
    let render_root = m.root () in
    let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    Iso.Data.source := (fun key _ ->
      if not (Hashtbl.mem pending key) && not (Hashtbl.mem attempted key)
         && not (Hashtbl.mem Iso.Data.seed key) then Hashtbl.replace pending key ());
    let rec passes n =
      let html = Iso.to_html (render_root ()) in
      if Hashtbl.length pending = 0 || n > 16 then html
      else begin
        let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
        Hashtbl.clear pending;
        Eio.Fiber.all (List.map (fun key () ->
          Eio.Time.sleep clock 0.02;            (* simulate latency; batch runs concurrently *)
          Hashtbl.replace attempted key ();
          match source key with Some v -> Iso.Data.put_seed key v | None -> ()) batch);
        passes (n + 1)
      end
    in
    let body = passes 0 in
    let ctx = { Iso.Doc.head = Iso.Head.to_ssr (); data = Iso.Data.to_script ();
                body; styles; client_js } in
    Some (Iso.document (document ctx))
