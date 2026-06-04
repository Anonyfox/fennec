(* See assets.mli. *)

type change = Nothing | Css_only | Reload

let classify ~css ~other = if other then Reload else if css then Css_only else Nothing

type t = { dir : string; hashes : (string, Digest.t) Hashtbl.t }

let create ~dir = { dir; hashes = Hashtbl.create 64 }

(* every served .css/.js/.mjs under [dir], recursively *)
let rec collect dir acc =
  match Sys.readdir dir with
  | exception _ -> acc
  | entries ->
    Array.fold_left
      (fun acc f ->
        let p = Filename.concat dir f in
        if (try Sys.is_directory p with _ -> false) then collect p acc
        else match Filename.extension f with ".css" | ".js" | ".mjs" -> p :: acc | _ -> acc)
      acc entries

let poll t =
  let css = ref false and other = ref false in
  let seen = Hashtbl.create 64 in
  List.iter
    (fun p ->
      Hashtbl.replace seen p ();
      match (try Some (Digest.file p) with _ -> None) with
      | None -> ()
      | Some h -> (
        match Hashtbl.find_opt t.hashes p with
        | Some old when old = h -> ()
        | _ ->
          Hashtbl.replace t.hashes p h;
          if Filename.extension p = ".css" then css := true else other := true))
    (collect t.dir []);
  (* tracked files that vanished this pass were deleted: forget them (so their hash can't linger
     forever) and force a full reload — a removed stylesheet/script changes the page, and a CSS
     hot-swap would just re-request a now-404 file. *)
  let deleted = Hashtbl.fold (fun p _ acc -> if Hashtbl.mem seen p then acc else p :: acc) t.hashes [] in
  if deleted <> [] then other := true;
  List.iter (Hashtbl.remove t.hashes) deleted;
  classify ~css:!css ~other:!other

let seed t = ignore (poll t)
