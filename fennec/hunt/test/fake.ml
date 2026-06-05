(* In-memory fake implementing the new {!Fennec_hunt.Backend.S}. The model is a static
   world: [wait cond] checks the condition against the world RIGHT NOW and returns Ok or a
   timeout Diag — fully deterministic, no clock, no polling. (Real evented waiting over
   time is a real-browser concern, covered by the dogfood; here we prove the DSL→condition
   mapping, short-circuiting, diagnostics, and the runner.) An [on_action] hook lets a
   click/fill mutate the world, modelling "the app reacting". *)

module Cond = Fennec_hunt.Backend.Cond
module Diag = Fennec_hunt.Backend.Diag

(* local substring test (the lib's internal Cdp.contains is not part of the public API) *)
let contains hay ndl =
  let hl = String.length hay and nl = String.length ndl in
  let rec matches i j = j = nl || (hay.[i + j] = ndl.[j] && matches i (j + 1)) in
  let rec scan i = i + nl <= hl && (matches i 0 || scan (i + 1)) in
  nl = 0 || scan 0

type world = {
  mutable url : string;
  mutable visible : string list;
  mutable texts : (string * string) list;
  mutable values : (string * string) list;
  mutable attrs : (string * string * string) list;
  mutable counts : (string * int) list;
  mutable clicked : string list;
  mutable filled : (string * string) list;
  mutable pressed : (string * string) list;
  (* test fixtures for the rich diagnostics: a present-but-not-shown element's specific
     reason, an element's outerHTML, captured console lines, and a multi-part selector probe *)
  mutable hidden : (string * Diag.reason) list;
  mutable outer : (string * string) list;
  mutable probe : (string * (string * bool) list) list;
  mutable logs : string list;
  mutable on_action : world -> unit;
}

let world () =
  { url = "/"; visible = []; texts = []; values = []; attrs = []; counts = [];
    clicked = []; filled = []; pressed = []; hidden = []; outer = []; probe = [];
    logs = []; on_action = (fun _ -> ()) }

type t = world

let is_vis w s = List.mem s w.visible
let is_present w s = is_vis w s || List.mem_assoc s w.texts || List.mem_assoc s w.counts
let n_match w s = match List.assoc_opt s w.counts with Some n -> n | None -> if is_vis w s then 1 else 0

let holds w (c : Cond.t) =
  match c with
  | Visible s | Actionable s -> is_vis w s
  | Hidden s -> not (is_vis w s)
  | Present s -> is_present w s
  | Detached s -> n_match w s = 0 && not (is_present w s)
  | Text (s, t) -> ( match List.assoc_opt s w.texts with Some x -> contains x t | None -> false )
  | Value (s, v) -> List.assoc_opt s w.values = Some v
  | Attr (s, n, v) -> List.exists (fun (a, b, c) -> a = s && b = n && c = v) w.attrs
  | Count (s, n) -> n_match w s = n
  | Url u -> contains w.url u
  | Js _ -> false (* the fake cannot evaluate JS; such conditions are real-browser only *)

(* Pick the precise [Diag.reason] for an unmet condition, mirroring what the real in-page
   harness computes — so the DSL→reason mapping and the formatter are proven on the fake,
   deterministically, for every failure mode. *)
let diag w (c : Cond.t) : Diag.t =
  let sel = Cond.selector c in
  let matched = match sel with Some s -> n_match w s | None -> -1 in
  let outer_html = match sel with Some s -> List.assoc_opt s w.outer | None -> None in
  let probe = match sel with Some s -> ( match List.assoc_opt s w.probe with Some p -> p | None -> [] ) | None -> [] in
  let reason : Diag.reason =
    match c with
    | Visible s | Actionable s ->
      if not (is_present w s) then Diag.No_match
      else ( match List.assoc_opt s w.hidden with Some r -> r | None -> Diag.Hidden_display "none" )
    | Present _ -> Diag.No_match
    | Hidden _ -> Diag.Still_visible
    | Detached s -> Diag.Still_present (n_match w s)
    | Text (s, _) -> ( match List.assoc_opt s w.texts with Some x -> Diag.Text_mismatch x | None -> Diag.No_match )
    | Value (s, _) -> if not (is_present w s) then Diag.No_match else Diag.Value_mismatch (List.assoc_opt s w.values)
    | Attr (s, n, _) ->
      if not (is_present w s) then Diag.No_match
      else ( match List.find_map (fun (a, b, cc) -> if a = s && b = n then Some cc else None) w.attrs with
             | Some actual -> Diag.Attr_mismatch actual | None -> Diag.Attr_absent )
    | Count _ -> Diag.Wrong_count (if matched < 0 then 0 else matched)
    | Url _ -> Diag.Url_mismatch w.url
    | Js _ -> Diag.Js_false
  in
  Diag.make ~selector:sel ~matched ~outer_html ~probe ~url:w.url ~ready:"complete" ~logs:w.logs reason

let navigate w ~url ~timeout:_ = w.url <- url; Ok ()
let wait w c ~timeout:_ = if holds w c then Ok () else Error (diag w c)

let click w ~selector = w.clicked <- selector :: w.clicked; w.on_action w
let fill w ~selector ~value =
  w.filled <- (selector, value) :: w.filled;
  w.values <- (selector, value) :: List.remove_assoc selector w.values;
  w.on_action w
let press w ~selector ~key = w.pressed <- (selector, key) :: w.pressed; w.on_action w

let read_text w ~selector = List.assoc_opt selector w.texts
let read_value w ~selector = List.assoc_opt selector w.values
let read_attr w ~selector ~name = List.find_map (fun (s, n, v) -> if s = selector && n = name then Some v else None) w.attrs
let read_count w ~selector = n_match w selector
let current_url w = w.url
let eval _ _ = ""

(* compile-time proof the fake honours the exact same contract as the real backend *)
module _ : Fennec_hunt.Backend.S = struct
  type nonrec t = t
  let navigate = navigate and wait = wait and click = click and fill = fill and press = press
  and read_text = read_text and read_value = read_value and read_attr = read_attr
  and read_count = read_count and current_url = current_url and eval = eval
end
