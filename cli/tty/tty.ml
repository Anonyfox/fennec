(* See tty.mli. Terminal capability detection + the low-level escape-sequence helpers the UI
   builds on. All detection happens ONCE at startup; everything else is pure string-building. *)

type t = { color : bool; hyperlinks : bool; interactive : bool; width : int }

let getenv k = match Sys.getenv_opt k with Some v -> v | None -> ""
let present k = match Sys.getenv_opt k with Some v -> v <> "" | None -> false

(* COLUMNS, else `tput cols`, else 80. One cheap shell-out at startup; never on the hot path. *)
let detect_width () =
  match Option.bind (Sys.getenv_opt "COLUMNS") int_of_string_opt with
  | Some n when n > 0 -> n
  | _ -> (
    match (try Some (input_line (Unix.open_process_in "tput cols 2>/dev/null")) with _ -> None) with
    | Some s -> ( match int_of_string_opt (String.trim s) with Some n when n > 0 -> n | _ -> 80)
    | None -> 80)

(* OSC 8 hyperlink support, by terminal allow-list. Unknown terminals get a plain (still
   copy-pasteable) URL rather than risking escape soup — conservative on purpose. *)
let supports_hyperlinks () =
  if present "CI" then false
  else
    match getenv "TERM_PROGRAM" with
    | "iTerm.app" | "WezTerm" | "ghostty" | "vscode" | "Hyper" -> true
    | "Apple_Terminal" -> false (* Terminal.app has no OSC 8 *)
    | _ -> present "WT_SESSION" || present "KITTY_WINDOW_ID" || (match int_of_string_opt (getenv "VTE_VERSION") with Some v -> v >= 5000 | None -> false)

let detect () =
  let interactive = try Unix.isatty Unix.stdout with _ -> false in
  (* colour precedence (documented): NO_COLOR > FORCE_COLOR > is-a-tty *)
  let color = if present "NO_COLOR" then false else if present "FORCE_COLOR" then true else interactive in
  { color; hyperlinks = color && interactive && supports_hyperlinks (); interactive; width = (if interactive then detect_width () else 80) }

(* a plain capability set for non-interactive sinks (pipes, CI, tests) *)
let plain = { color = false; hyperlinks = false; interactive = false; width = 80 }

(* ---- pure string builders ---- *)

let sgr t code s = if t.color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s

(* OSC 8: ESC ] 8 ;; URI ST  text  ESC ] 8 ;; ST  — falls back to plain [text] when unsupported *)
let hyperlink t ~url ~text = if t.hyperlinks then Printf.sprintf "\027]8;;%s\027\\%s\027]8;;\027\\" url text else text

(* live-region control (only meaningful when interactive; emitted as-is) *)
let cursor_up n = if n <= 0 then "" else Printf.sprintf "\027[%dA" n
let erase_below = "\027[J" (* erase from cursor to end of screen *)
let cr = "\r"

(* visible length: bytes minus ANSI SGR and OSC 8 sequences, and counting a UTF-8 codepoint as 1
   (good enough for our glyphs; not full grapheme width). Used to truncate to the terminal width. *)
let visible_width (s : string) : int =
  let n = String.length s and i = ref 0 and w = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\027' then (
      (* skip CSI (ESC[ … letter) or OSC 8 (ESC] … ST) *)
      if !i + 1 < n && s.[!i + 1] = '[' then (
        i := !i + 2;
        while !i < n && not (s.[!i] >= '@' && s.[!i] <= '~') do incr i done;
        if !i < n then incr i)
      else if !i + 1 < n && s.[!i + 1] = ']' then (
        i := !i + 2;
        while !i < n && not (s.[!i] = '\007' || (s.[!i] = '\027' && !i + 1 < n && s.[!i + 1] = '\\')) do incr i done;
        if !i < n && s.[!i] = '\027' then i := !i + 2 else if !i < n then incr i)
      else incr i)
    else (
      (* count one per UTF-8 lead byte (skip continuation bytes 0x80..0xBF) *)
      if Char.code c < 0x80 || Char.code c >= 0xC0 then incr w;
      incr i)
  done;
  !w
