open Js_of_ocaml
open Iso

type melem = { tag : string; key : string option; node : Dom_html.element Js.t;
               mutable attrs : attr list; mutable children : mounted list;
               handlers : (string, (unit -> unit) ref) Hashtbl.t }
and mcomp = { mcid : string; mckey : string option; mutable msub : mounted; meff : Iso.reaction;
              cleanups : (unit -> unit) list ref }
and mounted = MText of Dom.text Js.t | MElem of melem | MComp of mcomp

let doc = Dom_html.document
let rec mnode = function
  | MText t -> (t :> Dom.node Js.t) | MElem e -> (e.node :> Dom.node Js.t) | MComp mc -> mnode mc.msub

(* ambient current event: set around each dispatch so Iso.target_value / key / etc.
   can read the live event without threading it through the (unit -> unit) handler.
   Installed once here (client only) over the real js_of_ocaml event. *)
let cur_event : Js.Unsafe.any option ref = ref None
let () =
  let get name = match !cur_event with None -> None | Some e -> (try Some (Js.Unsafe.get e (Js.string name)) with _ -> None) in
  Iso.ev_value := (fun () -> match get "target" with Some t -> (try Js.to_string (Js.Unsafe.get t (Js.string "value")) with _ -> "") | None -> "");
  Iso.ev_checked := (fun () -> match get "target" with Some t -> (try Js.to_bool (Js.Unsafe.get t (Js.string "checked")) with _ -> false) | None -> false);
  Iso.ev_key := (fun () -> match !cur_event with Some e -> (try Js.to_string (Js.Unsafe.get e (Js.string "key")) with _ -> "") | None -> "");
  Iso.ev_prevent := (fun () -> match !cur_event with Some e -> (try ignore (Js.Unsafe.meth_call e "preventDefault" [||]) with _ -> ()) | None -> ())

let ensure_handler handlers (node : Dom_html.element Js.t) ev f =
  match Hashtbl.find_opt handlers ev with
  | Some r -> r := f
  | None -> let r = ref f in Hashtbl.replace handlers ev r;
    ignore (Dom.addEventListener node (Dom.Event.make ev)
      (Dom.handler (fun e ->
        cur_event := Some (Js.Unsafe.inject e);
        Fun.protect ~finally:(fun () -> cur_event := None) (fun () -> !r ());
        Js._true)) Js._false)

(* value/checked are live DOM PROPERTIES, not attributes — set them as properties so
   controlled inputs actually update (and clear) after the user has typed. *)
let set_attr (node : Dom_html.element Js.t) handlers = function
  | Attr ("value", v) -> Js.Unsafe.set node (Js.string "value") (Js.string v)
  | Attr ("checked", v) -> Js.Unsafe.set node (Js.string "checked") (Js.bool (v <> ""))
  | Attr (k,v) -> node##setAttribute (Js.string k) (Js.string v)
  | Handler (ev,f) -> ensure_handler handlers node ev f
let key_of_m = function MElem e -> e.key | MComp mc -> mc.mckey | _ -> None
let key_of_v = function Elem {key;_} -> key | Comp {ckey;_} -> ckey | _ -> None
let has_key v = key_of_v v <> None

let rec create = function
  | Text s -> MText (doc##createTextNode (Js.string s))
  | Comp c -> mount_comp c
  | Fragment _ -> MText (doc##createTextNode (Js.string ""))
  | Elem { tag; key; attrs; children } ->
    let node = doc##createElement (Js.string tag) in
    let handlers = Hashtbl.create 4 in
    List.iter (set_attr node handlers) attrs;
    let children = List.map (fun ch -> let m = create ch in Dom.appendChild node (mnode m); m) (Iso.flatten children) in
    MElem { tag; key; node; attrs; children; handlers }

and mk_effect ~first sub =
  { Iso.run = (fun () -> match !sub with `Render (render, m) ->
      let v = render () in
      (match m with
       | None -> sub := `Render (render, Some (first v))
       | Some old -> let p = Js.Opt.get (mnode old)##.parentNode (fun () -> failwith "detached") in
         sub := `Render (render, Some (reconcile ~parent:p old v)))) ; deps = [] }

and instantiate ~first (c : Iso.comp) =
  (* scope cleanups to this instance: setup + first render register into [cleanups]
     (save/restore so nested children scope to themselves) *)
  let cleanups = ref [] in
  let saved = !Iso.current_cleanups in
  Iso.current_cleanups := cleanups;
  let render = c.setup () in
  let sub = ref (`Render (render, None)) in
  let eff = mk_effect ~first sub in
  Iso.run_effect eff;
  Iso.current_cleanups := saved;
  let m = (match !sub with `Render (_, Some m) -> m | _ -> failwith "comp produced nothing") in
  MComp { mcid = c.cid; mckey = c.ckey; msub = m; meff = eff; cleanups }
and mount_comp c = instantiate ~first:create c
and hydrate_comp dom c = instantiate ~first:(fun v -> hydrate dom v) c

and hydrate (dom : Dom.node Js.t) = function
  | Text _ -> MText (Js.Unsafe.coerce dom)
  | Comp c -> hydrate_comp dom c
  | Fragment _ -> MText (Js.Unsafe.coerce dom)
  | Elem { tag; key; attrs; children } ->
    let node : Dom_html.element Js.t = Js.Unsafe.coerce dom in
    let handlers = Hashtbl.create 4 in
    List.iter (function Handler (ev,f) -> ensure_handler handlers node ev f | Attr _ -> ()) attrs;
    let dn = node##.childNodes in
    (* adopt the SSR'd child if present; on a SSR/CSR mismatch, recover by creating
       it and appending (never crash the page over a desync) *)
    let children = List.mapi (fun i ch ->
      match Js.Opt.to_option (dn##item i) with
      | Some d -> hydrate d ch
      | None -> let m = create ch in Dom.appendChild node (mnode m); m) (Iso.flatten children) in
    MElem { tag; key; node; attrs; children; handlers }

and unmount = function
  | MComp mc ->
    List.iter (fun f -> f ()) !(mc.cleanups);  (* run instance cleanups (Head.use removal, etc.) *)
    Iso.dispose mc.meff; unmount mc.msub
  | MElem e -> List.iter unmount e.children
  | MText _ -> ()

and patch_attrs e new_attrs =
  List.iter (function
    | Attr ("value", v) -> if Js.to_string (Js.Unsafe.get e.node (Js.string "value")) <> v then Js.Unsafe.set e.node (Js.string "value") (Js.string v)
    | Attr ("checked", v) -> Js.Unsafe.set e.node (Js.string "checked") (Js.bool (v <> ""))
    | Attr (k,v) ->
      let cur = e.node##getAttribute (Js.string k) in
      if not (Js.Opt.test cur) || Js.to_string (Js.Opt.get cur (fun()->Js.string "")) <> v then
        e.node##setAttribute (Js.string k) (Js.string v)
    | Handler (ev,f) -> ensure_handler e.handlers e.node ev f) new_attrs;
  List.iter (function
    | Attr (k,_) when not (List.exists (function Attr (k2,_) -> k2=k | _ -> false) new_attrs) -> e.node##removeAttribute (Js.string k)
    | _ -> ()) e.attrs

and reconcile ~(parent : Dom.node Js.t) m vnode : mounted =
  match m, vnode with
  | MText t, Text s -> (if Js.to_string t##.data <> s then t##.data := Js.string s); m
  | MElem e, Elem { tag; attrs; children; _ } when e.tag = tag ->
    patch_attrs e attrs; e.attrs <- attrs;
    e.children <- reconcile_children ~parent:(e.node :> Dom.node Js.t) e.children (Iso.flatten children); m
  | MComp mc, Comp c when mc.mcid = c.cid && mc.mckey = c.ckey -> m  (* keep instance: its own effect drives it *)
  | _ -> unmount m; let m' = create vnode in Dom.replaceChild parent (mnode m') (mnode m); m'

and reconcile_children ~parent olds news =
  if List.exists has_key news then keyed ~parent olds news else positional ~parent olds news
and positional ~parent olds news = match olds, news with
  | o :: os, n :: ns -> let m = reconcile ~parent o n in m :: positional ~parent os ns
  | [], n :: ns -> let m = create n in Dom.appendChild parent (mnode m); m :: positional ~parent [] ns
  | o :: os, [] -> unmount o; Dom.removeChild parent (mnode o); positional ~parent os []
  | [], [] -> []
and keyed ~parent olds news =
  let map = Hashtbl.create 16 in
  List.iter (fun m -> match key_of_m m with Some k -> Hashtbl.replace map k m | None -> ()) olds;
  let used = Hashtbl.create 16 in
  let result = List.map (fun vn -> match key_of_v vn with
      | Some k when Hashtbl.mem map k -> Hashtbl.replace used k (); reconcile ~parent (Hashtbl.find map k) vn
      | _ -> create vn) news in
  List.iter (fun m -> Dom.appendChild parent (mnode m)) result;
  List.iter (fun m -> match key_of_m m with Some k when not (Hashtbl.mem used k) -> unmount m; Dom.removeChild parent (mnode m) | _ -> ()) olds;
  result

let hydrate_root (container : Dom_html.element Js.t) (render : unit -> vnode) =
  let mounted = ref None in
  let eff = { Iso.run = (fun () ->
      let vnode = render () in
      match !mounted with
      | None -> let first = Js.Opt.get container##.firstChild (fun () -> failwith "no SSR root") in mounted := Some (hydrate first vnode)
      | Some m -> mounted := Some (reconcile ~parent:(container :> Dom.node Js.t) m vnode));
     deps = [] } in
  Iso.run_effect eff
