(* Server render driver. Dispatches a request to a mounted app (longest base wins,
   strips the base), runs its data fetches concurrently between render passes (Eio),
   then assembles the app's chosen document. Generic over the generated [mount list].

   Per-request isolation: the Data context (seed + source), the Head tag registry, AND the active Router
   are ALL per-request — both [render] and the synchronous [handler] wrap in [Fur.Data.with_context] and
   activate a per-request CLONE of the app router (its own path signal), so concurrent renders across
   fibers AND domains never share or leak title/meta, data seeds, or the active route. *)
let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dispatch (mounts : Fur.mount list) path =
  List.filter (fun (m : Fur.mount) -> m.base = "" || path = m.base || starts_with (m.base ^ "/") path) mounts
  |> List.sort (fun (a : Fur.mount) b -> compare (String.length b.base) (String.length a.base))
  |> function m :: _ -> Some m | [] -> None

(* ── boot-time data warm-up ─────────────────────────────────────────────────────────────────────
   A co-located {!Fur.Data.local} resource registers its server fetcher when the declaring component's
   SETUP first runs — which is during a render, not at process start. So at boot the registry can be
   empty and the auto-mounted refetch route would 404 until the first page render. To make the route
   deterministic from the first request, every [handler]/[render] EAGERLY records its mounts here (the
   labeled-args application runs this before the per-request function is returned), and {!warm_data}
   does ONE throwaway render of each recorded mount in a discarded data context — running every
   component's setup once, which populates the registry. The render output is dropped; this only forces
   the [Data.local] registrations. Idempotent + cheap (each app rendered once at boot). *)
let _recorded_mounts : Fur.mount list ref = ref []
let record_mounts (mounts : Fur.mount list) =
  List.iter (fun (m : Fur.mount) -> if not (List.memq m !_recorded_mounts) then _recorded_mounts := m :: !_recorded_mounts) mounts

let warm_data () =
  List.iter
    (fun (m : Fur.mount) ->
      (* a fully isolated, discarded render context — same machinery as a real SSR request, but the
         HTML is thrown away. Its only purpose is to run component setup so [Data.local] registers. *)
      Fur.Data.with_context (fun () ->
          let router = Fur.Router.clone_for_render m.Fur.router in
          Fur.Router.activate router;
          Fur.Router.set_path router m.Fur.base;
          (try ignore (Fur.to_html ((m.Fur.root ()) ())) with _ -> ())))
    !_recorded_mounts

let render ?(head_extra = "") ~env ~(mounts : Fur.mount list) ~source ~request ~client_js ~styles () : string option =
  record_mounts mounts;   (* idempotent; lets boot-time {!warm_data} register co-located sources *)
  match dispatch mounts request with
  | None -> None
  | Some m ->
    (* per-request isolation: each concurrent render gets its own fiber-local seed table + source *)
    Fur.Data.with_context @@ fun () ->
    let clock = Eio.Stdenv.clock env in
    let router = Fur.Router.clone_for_render m.router in  (* per-request: its OWN path signal + params *)
    Fur.Router.activate router;
    Fur.Router.set_path router request;
    let render_root = m.root () in
    (* chain the explicit [~source] with the co-located {!Fur.Data.local} registry (see [handler]) *)
    let effective_source key = match source key with Some _ as hit -> hit | None -> Option.map Fur.Data.src_produce (Fur.Data.local_source key) in
    let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    Fur.Data.set_source (fun key _ ->
      if not (Hashtbl.mem pending key) && not (Hashtbl.mem attempted key)
         && not (Hashtbl.mem (Fur.Data.seed_table ()) key) then Hashtbl.replace pending key ());
    let rec passes n =
      let html = Fur.to_html (render_root ()) in
      if Hashtbl.length pending = 0 || n > 16 then html
      else begin
        let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
        Hashtbl.clear pending;
        Eio.Fiber.all (List.map (fun key () ->
          Eio.Time.sleep clock 0.02;
          Hashtbl.replace attempted key ();
          match effective_source key with Some v -> Fur.Data.put_seed key v | None -> ()) batch);
        passes (n + 1)
      end
    in
    let body = passes 0 in
    let ctx = { Fur.Doc.head = Fur.Head.to_ssr () ^ head_extra; data = Fur.Data.to_script (); body; styles; client_js } in
    Some (Fur.document (m.document ctx))

(* Synchronous render — no Eio I/O (the [?source] fetcher is in-process). This is the shape
   [Endpoint.app] consumes directly (path -> html option). It STILL wraps in [Fur.Data.with_context]:
   a pure render does not yield, but it DOES run in parallel across domains on the real server, so the
   per-request globals it touches (Data seed + Head registry) must be fiber-local or two concurrent
   renders would race on — and leak — them. (Router active/current is set fresh below each call.)

   [?source] turns on server-side data: an in-process (path -> string option) fetcher
   run between render passes (synchronously — fine because the source is in-process; an
   app needing real async I/O uses [render ~env] above). Each pass renders, collects the
   data keys the tree asked for, fetches + seeds them, and re-renders until the tree
   stops asking (or 16 passes). The seed is embedded for fast-render on the client.

   [?styles] is inlined into the document <style> (the extracted, scoped [%%style] of the
   app's components). Asset URLs (js/css <link>/<script>) stay the document template's
   concern; ctx.client_js is left empty (external bundle, served by the framework). *)
let handler ?(styles = "") ?(head_extra = "") ?source ~(mounts : Fur.mount list) : string -> string option =
  record_mounts mounts;   (* eager (runs at this labeled-args application): enables boot-time {!warm_data} *)
  fun (request : string) ->
  match dispatch mounts request with
  | None -> None
  | Some m ->
    (* per-request isolation: each render gets its own fiber-local seed table + Head registry + a clone
       of the app router (its own path signal), so parallel renders across domains never share or leak
       any of them — title/meta, data seeds, OR the active route *)
    Fur.Data.with_context @@ fun () ->
    let router = Fur.Router.clone_for_render m.router in
    Fur.Router.activate router;
    Fur.Router.set_path router request;
    let render_root = m.root () in
    (* the effective SSR source CHAINS the app's explicit [?source] (legacy stringly keys) with the
       co-located {!Fur.Data.local} registry: a resource declared INLINE in a component seeds itself with
       NO server.ml [api_source] arm. The explicit source wins (an app can still override a path); a
       co-located resource fills the gap. With no [?source] AND an empty registry this is the no-data
       single-pass render, byte-identical to before. *)
    let effective_source key =
      match (match source with Some s -> s key | None -> None) with
      | Some _ as hit -> hit
      | None -> Option.map Fur.Data.src_produce (Fur.Data.local_source key)
    in
    let pending : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    let attempted : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    Fur.Data.set_source
      (fun key _ ->
        if (not (Hashtbl.mem pending key)) && (not (Hashtbl.mem attempted key))
           && not (Hashtbl.mem (Fur.Data.seed_table ()) key)
        then Hashtbl.replace pending key ());
    let rec passes n =
      ignore (Fur.to_html (render_root ()));
      if Hashtbl.length pending = 0 || n > 16 then ()
      else begin
        let batch = Hashtbl.fold (fun k () acc -> k :: acc) pending [] in
        Hashtbl.clear pending;
        List.iter
          (fun key ->
            Hashtbl.replace attempted key ();
            match effective_source key with Some v -> Fur.Data.put_seed key v | None -> ())
          batch;
        passes (n + 1)
      end
    in
    passes 0;
    Fur.Data.set_source (fun _ _ -> ());
    let body = Fur.to_html (render_root ()) in
    let ctx =
      { Fur.Doc.head = Fur.Head.to_ssr () ^ head_extra; data = Fur.Data.to_script ();
        body; styles; client_js = "" }
    in
    Some (Fur.document (m.document ctx))
