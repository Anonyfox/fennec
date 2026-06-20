(* [fennec doctor] — diagnose the .mlx dialect toolchain reaching the user's machine. See doctor.mli.

   Split into (1) PURE report-building over already-gathered facts (unit-tested with no IO) and
   (2) thin IO probes (`command -v`, the dune-project read). The pure half is the part that can be
   wrong (which remedy, the all-ok verdict), so it is the part the tests pin down. *)

type tool = {
  name : string;
  found : bool;
  required : bool;
  purpose : string;
  remedy : string;
}

type project = { dune_project : string option; dialect_ok : bool }

(* The verbatim stanza we tell the user to paste — kept identical to the workspace's own dune-project
   and to what `fennec new` emits, so "what doctor prints" and "what works" can never drift. *)
let stanza =
  "(dialect\n\
  \ (name mlx)\n\
  \ (implementation\n\
  \  (extension mlx)\n\
  \  (merlin_reader fennec-mlx)\n\
  \  (preprocess\n\
  \   (run fennec-mlx-pp %{input-file}))))"

(* ── PURE: does a dune-project text declare the mlx dialect? ──────────────────────────────────────
   We do not parse s-expressions (no sexp lib in this library, and a substring scan is robust enough):
   find a `(dialect` token, then within the parenthesised group that opens there, look for a `(name`
   whose first atom is `mlx`. Tolerant of arbitrary whitespace/newlines between tokens. A commented
   line (`;`) inside the group is skipped so a `; (dialect …)` note never counts as a declaration. *)
let text_declares_dialect (text : string) : bool =
  let n = String.length text in
  (* strip line comments: dune uses `;` to end-of-line *)
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if text.[!i] = ';' then (
      while !i < n && text.[!i] <> '\n' do incr i done)
    else (Buffer.add_char buf text.[!i]; incr i)
  done;
  let s = Buffer.contents buf in
  let sn = String.length s in
  (* scan for the next occurrence of [needle] at or after [from], returning its index or -1 *)
  let find_from needle from =
    let nl = String.length needle in
    let rec go k = if k + nl > sn then -1 else if String.sub s k nl = needle then k else go (k + 1) in
    go (max 0 from)
  in
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let skip_ws k = let k = ref k in while !k < sn && is_ws s.[!k] do incr k done; !k in
  (* check whether a `(dialect` group starting at [open_paren] contains a `(name mlx …)` *)
  let group_has_mlx_name open_paren =
    (* find the matching close paren for [open_paren] (depth scan) *)
    let depth = ref 0 and j = ref open_paren and close = ref (-1) in
    while !j < sn && !close < 0 do
      (match s.[!j] with '(' -> incr depth | ')' -> decr depth; if !depth = 0 then close := !j | _ -> ());
      incr j
    done;
    if !close < 0 then false
    else begin
      (* within [open_paren, close], find a `(name` then assert the next atom is `mlx` *)
      let limit = !close in
      let rec scan from =
        let k = find_from "(name" from in
        if k < 0 || k >= limit then false
        else begin
          let after = skip_ws (k + String.length "(name") in
          (* read the atom *)
          let e = ref after in
          while !e < sn && not (is_ws s.[!e]) && s.[!e] <> ')' && s.[!e] <> '(' do incr e done;
          let atom = String.sub s after (!e - after) in
          if atom = "mlx" then true else scan (k + 1)
        end
      in
      scan open_paren
    end
  in
  let rec scan from =
    let k = find_from "(dialect" from in
    if k < 0 then false
    else if group_has_mlx_name k then true
    else scan (k + 1)
  in
  scan 0

(* ── PURE: render the report ─────────────────────────────────────────────────────────────────────
   Layout: a header, one line per tool (✓/✗ + name + purpose), then the dialect-stanza line, then —
   if anything is wrong — a REMEDIES block with the exact fix per missing piece, else a green OK. *)
let render (tty : Tty.t) ~(tools : tool list) ~(project : project) : string * bool =
  let green s = Tty.sgr tty "32" s in
  let red s = Tty.sgr tty "31" s in
  let yellow s = Tty.sgr tty "33" s in
  let dim s = Tty.sgr tty "2" s in
  let bold s = Tty.sgr tty "1" s in
  let ok_mark = green "✓" and bad_mark s = if s then red "✗" else yellow "•" in
  let b = Buffer.create 1024 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  line "%s" (bold "fennec doctor — .mlx toolchain check");
  line "";
  (* tools *)
  List.iter
    (fun t ->
      let mark = if t.found then ok_mark else bad_mark t.required in
      let status = if t.found then "" else if t.required then dim "  (missing — required)" else dim "  (missing — optional)" in
      line "  %s %-26s %s%s" mark t.name (dim t.purpose) status)
    tools;
  (* dialect stanza *)
  let dialect_mark = if project.dialect_ok then ok_mark else bad_mark true in
  let where = match project.dune_project with Some p -> p | None -> "(no dune-project found)" in
  line "  %s %-26s %s" dialect_mark "(dialect (name mlx) …)" (dim (Printf.sprintf "in %s" where));
  (* verdict + remedies *)
  let missing_required = List.filter (fun t -> t.required && not t.found) tools in
  let missing_optional = List.filter (fun t -> (not t.required) && not t.found) tools in
  let dialect_bad = not project.dialect_ok in
  let all_ok = missing_required = [] && not dialect_bad in
  line "";
  if all_ok then begin
    line "%s" (green "toolchain OK — your frontend .mlx app will build and the editor will light up.");
    if missing_optional <> [] then
      List.iter
        (fun t -> line "%s %s" (yellow "note:") (Printf.sprintf "%s is absent (optional) — %s" t.name t.remedy))
        missing_optional
  end
  else begin
    line "%s" (red "toolchain INCOMPLETE — fix the items below, then re-run `fennec doctor`.");
    line "";
    line "%s" (bold "remedies:");
    (* dedupe identical remedies (e.g. mlx-pp + ocamlmerlin-mlx both come from `opam install mlx`) *)
    let seen = Hashtbl.create 8 in
    let emit_remedy label remedy =
      let key = remedy in
      if not (Hashtbl.mem seen key) then begin
        Hashtbl.add seen key ();
        line "  %s" (bold ("• " ^ label));
        List.iter (fun l -> line "      %s" l) (String.split_on_char '\n' remedy)
      end
    in
    List.iter (fun t -> emit_remedy (t.name ^ " (missing)") t.remedy) missing_required;
    if dialect_bad then
      emit_remedy "dune-project is missing the mlx dialect stanza — paste this at top level:"
        stanza;
    (* optional tools: list after the required fixes, as warnings, since they don't block the build *)
    if missing_optional <> [] then begin
      line "";
      List.iter
        (fun t -> line "  %s %s" (yellow "(optional)") (Printf.sprintf "%s — %s" t.name t.remedy))
        missing_optional
    end
  end;
  (Buffer.contents b, all_ok)

(* ── IO: probes ──────────────────────────────────────────────────────────────────────────────────
   `command -v NAME` via /bin/sh: POSIX, honours the inherited PATH exactly as the dialect's spawn of
   mlx-pp does, and never executes the tool. Redirect its output so nothing leaks to our stdout. *)
let which (name : string) : bool =
  (* guard the name (only our fixed literals reach here, but stay safe) *)
  let safe = String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-' || c = '_' || c = '.') name in
  if not safe then false
  else
    let cmd = Printf.sprintf "command -v %s >/dev/null 2>&1" name in
    match Unix.system cmd with Unix.WEXITED 0 -> true | _ -> false

(* find the nearest dune-project from [dir] upward (the project we'd build from) *)
let rec find_dune_project dir =
  let candidate = Filename.concat dir "dune-project" in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_dune_project parent

let read_file path = try In_channel.with_open_bin path In_channel.input_all with _ -> ""

(* the tools we check, with their fennec-specific remedies *)
let probe_tools () : tool list =
  [ { name = "fennec-mlx-pp"; required = true;
      purpose = "the .mlx build preprocessor";
      found = which "fennec-mlx-pp";
      remedy = "opam install fennec-cli   (ships fennec-mlx-pp + ocamlmerlin-fennec-mlx)" };
    { name = "ocamlmerlin-fennec-mlx"; required = true;
      purpose = "the editor merlin reader for .mlx";
      found = which "ocamlmerlin-fennec-mlx";
      remedy = "opam install fennec-cli   (ships fennec-mlx-pp + ocamlmerlin-fennec-mlx)" };
    { name = "mlx-pp"; required = true;
      purpose = "the stock mlx parser (fennec-mlx-pp pipes through it)";
      found = which "mlx-pp";
      remedy = "opam install mlx" };
    { name = "ocamlmerlin-mlx"; required = true;
      purpose = "the stock mlx editor reader (delegated to)";
      found = which "ocamlmerlin-mlx";
      remedy = "opam install mlx" };
    { name = "node"; required = false;
      purpose = "JS runtime esbuild can use for npm deps";
      found = which "node";
      remedy = "install Node.js (https://nodejs.org) — only needed if your bundle imports npm packages" } ]

let run () : int =
  let tty = Tty.detect () in
  let tools = probe_tools () in
  let dune_project = find_dune_project (Sys.getcwd ()) in
  let dialect_ok = match dune_project with Some p -> text_declares_dialect (read_file p) | None -> false in
  let project = { dune_project; dialect_ok } in
  let text, all_ok = render tty ~tools ~project in
  print_string text;
  flush stdout;
  if all_ok then 0 else 1

(* ── tests (pure render + the dialect detector) ─────────────────────────────────────────────────── *)

let%test "dialect detector accepts the canonical stanza" =
  text_declares_dialect stanza

let%test "dialect detector accepts the workspace-style multiline stanza" =
  let t =
    "(lang dune 3.16)\n(generate_opam_files true)\n\n(dialect\n (name mlx)\n (implementation\n  (extension mlx)\n  (merlin_reader fennec-mlx)\n  (preprocess\n   (run fennec-mlx-pp %{input-file}))))\n\n(package (name foo))"
  in
  text_declares_dialect t

let%test "dialect detector rejects a dune-project with no dialect" =
  not (text_declares_dialect "(lang dune 3.16)\n(package (name foo))")

let%test "dialect detector rejects a NON-mlx dialect" =
  not (text_declares_dialect "(dialect (name reason) (implementation (extension re)))")

let%test "dialect detector ignores a commented-out stanza" =
  not (text_declares_dialect "(lang dune 3.16)\n; (dialect (name mlx))\n(package (name foo))")

let%test "dialect detector handles dialect-before-name with other names between" =
  (* a (dialect …) whose body lists another (name …) atom before mlx — still found *)
  text_declares_dialect "(dialect\n (foo bar)\n (name mlx))"

(* render: all present, with a dialect stanza → OK + green verdict *)
let all_present_tools =
  [ { name = "fennec-mlx-pp"; found = true; required = true; purpose = "p"; remedy = "r" };
    { name = "mlx-pp"; found = true; required = true; purpose = "p"; remedy = "opam install mlx" };
    { name = "node"; found = true; required = false; purpose = "p"; remedy = "r" } ]

let%test "render: all present + dialect ⇒ all_ok=true, says toolchain OK" =
  let text, ok = render Tty.plain ~tools:all_present_tools ~project:{ dune_project = Some "/x/dune-project"; dialect_ok = true } in
  ok && (let re s = let n = String.length s and m = String.length text in let rec go i = i + n <= m && (String.sub text i n = s || go (i+1)) in go 0 in re "toolchain OK")

(* render: a required tool missing ⇒ not ok, remedy printed *)
let%test "render: missing required mlx-pp ⇒ all_ok=false + prints `opam install mlx`" =
  let tools =
    [ { name = "fennec-mlx-pp"; found = true; required = true; purpose = "p"; remedy = "opam install fennec-cli" };
      { name = "mlx-pp"; found = false; required = true; purpose = "p"; remedy = "opam install mlx" } ]
  in
  let text, ok = render Tty.plain ~tools ~project:{ dune_project = Some "/x/dune-project"; dialect_ok = true } in
  let re s = let n = String.length s and m = String.length text in let rec go i = i + n <= m && (String.sub text i n = s || go (i+1)) in go 0 in
  (not ok) && re "opam install mlx" && re "INCOMPLETE"

(* render: dialect stanza missing ⇒ not ok, the verbatim stanza is printed *)
let%test "render: missing dialect stanza ⇒ all_ok=false + prints the paste-in stanza" =
  let text, ok = render Tty.plain ~tools:all_present_tools ~project:{ dune_project = Some "/x/dune-project"; dialect_ok = false } in
  let re s = let n = String.length s and m = String.length text in let rec go i = i + n <= m && (String.sub text i n = s || go (i+1)) in go 0 in
  (not ok) && re "(merlin_reader fennec-mlx)" && re "(dialect"

(* render: an OPTIONAL tool missing does NOT fail the verdict *)
let%test "render: missing optional node ⇒ still all_ok=true" =
  let tools =
    [ { name = "mlx-pp"; found = true; required = true; purpose = "p"; remedy = "opam install mlx" };
      { name = "node"; found = false; required = false; purpose = "p"; remedy = "install node" } ]
  in
  let _text, ok = render Tty.plain ~tools ~project:{ dune_project = Some "/x/dune-project"; dialect_ok = true } in
  ok

(* render: identical remedies are de-duplicated (fennec-mlx-pp + ocamlmerlin both say install fennec-cli) *)
let%test "render: duplicate remedies appear once" =
  let tools =
    [ { name = "fennec-mlx-pp"; found = false; required = true; purpose = "p"; remedy = "opam install fennec-cli" };
      { name = "ocamlmerlin-fennec-mlx"; found = false; required = true; purpose = "p"; remedy = "opam install fennec-cli" } ]
  in
  let text, _ok = render Tty.plain ~tools ~project:{ dune_project = Some "/x/dune-project"; dialect_ok = true } in
  (* count occurrences of the remedy body line *)
  let count_sub s sub =
    let n = String.length sub and m = String.length s in
    let rec go i acc = if i + n > m then acc else if String.sub s i n = sub then go (i+1) (acc+1) else go (i+1) acc in
    go 0 0
  in
  count_sub text "opam install fennec-cli" = 1
