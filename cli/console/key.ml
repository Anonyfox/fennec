(* See key.mli. A small terminal-input decoder: single control bytes + the common CSI escape
   sequences (arrows, Home/End, Delete). Anything unrecognised becomes [Unknown] so it is simply
   ignored by the editor rather than corrupting the line. *)

type t =
  | Char of char
  | Enter
  | Backspace
  | Tab
  | Delete
  | Left
  | Right
  | Up
  | Down
  | Home
  | End
  | Ctrl of char
  | Esc
  | Unknown

(* parse the CSI sequence starting just after "ESC [" at index [i]; returns (key, next index). *)
let parse_csi s i =
  let n = String.length s in
  (* a CSI is "ESC [" then optional params (digits/';') then a final byte *)
  let j = ref i in
  while !j < n && s.[!j] >= '0' && s.[!j] <= ';' do incr j done;
  if !j >= n then (Esc, n)
  else
    let params = String.sub s i (!j - i) in
    let final = s.[!j] in
    let key =
      match final with
      | 'A' -> Up
      | 'B' -> Down
      | 'C' -> Right
      | 'D' -> Left
      | 'H' -> Home
      | 'F' -> End
      | '~' -> (
        match params with "1" | "7" -> Home | "4" | "8" -> End | "3" -> Delete | _ -> Unknown)
      | _ -> Unknown
    in
    (key, !j + 1)

let parse (s : string) : t list =
  let n = String.length s in
  let out = ref [] in
  let emit k = out := k :: !out in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    let code = Char.code c in
    if c = '\027' (* ESC *) then (
      if !i + 1 < n && s.[!i + 1] = '[' then (
        let key, next = parse_csi s (!i + 2) in
        emit key;
        i := next)
      else if !i + 1 < n && s.[!i + 1] = 'O' then (
        (* SS3 (application cursor mode): ESC O A/B/C/D/H/F *)
        let key = match (if !i + 2 < n then s.[!i + 2] else ' ') with
          | 'A' -> Up | 'B' -> Down | 'C' -> Right | 'D' -> Left | 'H' -> Home | 'F' -> End | _ -> Unknown in
        emit key;
        i := !i + 3)
      else (emit Esc; incr i))
    else if c = '\n' || c = '\r' then (emit Enter; incr i)
    else if code = 9 then (emit Tab; incr i)
    else if code = 127 || code = 8 then (emit Backspace; incr i)
    else if code >= 1 && code <= 26 then (emit (Ctrl (Char.chr (code - 1 + Char.code 'a'))); incr i)
    else if code >= 32 then (emit (Char c); incr i)
    else (emit Unknown; incr i)
  done;
  List.rev !out

let%test "printable text decodes to chars" =
  parse "ab" = [ Char 'a'; Char 'b' ]

let%test "control keys decode" =
  parse "\003\004\009\013\127" = [ Ctrl 'c'; Ctrl 'd'; Tab; Enter; Backspace ]

let%test "arrows + home/end/delete decode from CSI" =
  parse "\027[A\027[B\027[C\027[D\027[H\027[F\027[3~"
  = [ Up; Down; Right; Left; Home; End; Delete ]

let%test "a char after an escape sequence is not swallowed" =
  parse "\027[Ax" = [ Up; Char 'x' ]
