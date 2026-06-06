(* HTTP-date (RFC 7231 §7.1.1.1): format and parse the IMF-fixdate form
     "Sun, 06 Nov 1994 08:49:37 GMT"
   plus tolerant parsing of the two obsolete forms (RFC 850, asctime). Pure,
   Stdlib only. Times are UNIX epoch seconds (UTC). Used for Last-Modified /
   If-Modified-Since / Date / Expires. *)

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let months =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let month_of_string s =
  let rec go i = if i >= 12 then None else if months.(i) = s then Some i else go (i + 1) in
  go 0

(* Format epoch seconds as an IMF-fixdate. Uses Unix.gmtime. *)
let format (epoch : float) : string =
  let tm = Unix.gmtime epoch in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" days.(tm.Unix.tm_wday) tm.Unix.tm_mday
    months.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* ──── format ──── *)
let%test "format RFC example"   = format 784_111_777.0 = "Sun, 06 Nov 1994 08:49:37 GMT"
let%test "format epoch 0"       = format 0.0 = "Thu, 01 Jan 1970 00:00:00 GMT"
let%test "format length 29"     = String.length (format 1_700_000_000.0) = 29

(* Convert a broken-down UTC date to epoch seconds (days-since-epoch math; no
   timezone, all inputs are GMT). Returns None on out-of-range. *)
let to_epoch ~year ~mon ~day ~hour ~min ~sec : float option =
  if mon < 0 || mon > 11 || day < 1 || day > 31 || hour > 23 || min > 59 || sec > 60 then None
  else
    (* days from civil date — Howard Hinnant's algorithm *)
    let y = if mon <= 1 then year - 1 else year in
    let era = (if y >= 0 then y else y - 399) / 400 in
    let yoe = y - (era * 400) in
    let doy = ((153 * (if mon > 1 then mon - 2 else mon + 10)) + 2) / 5 + day - 1 in
    let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
    let days_since_epoch = (era * 146097) + doe - 719468 in
    Some
      (float_of_int ((days_since_epoch * 86400) + (hour * 3600) + (min * 60) + sec))

let parse_imf (s : string) : float option =
  (* "Sun, 06 Nov 1994 08:49:37 GMT" *)
  try
    Scanf.sscanf s "%3s, %d %3s %d %d:%d:%d GMT" (fun _wday day mon_s year hour min sec ->
        match month_of_string mon_s with
        | Some mon -> to_epoch ~year ~mon ~day ~hour ~min ~sec
        | None -> None)
  with _ -> None

let parse_asctime (s : string) : float option =
  (* "Sun Nov  6 08:49:37 1994" *)
  try
    Scanf.sscanf s "%3s %3s %d %d:%d:%d %d" (fun _wday mon_s day hour min sec year ->
        match month_of_string mon_s with
        | Some mon -> to_epoch ~year ~mon ~day ~hour ~min ~sec
        | None -> None)
  with _ -> None

let parse_rfc850 (s : string) : float option =
  (* "Sunday, 06-Nov-94 08:49:37 GMT" — 2-digit year, windowed to 1970..2069 *)
  try
    Scanf.sscanf s "%s@, %d-%3s-%d %d:%d:%d GMT" (fun _wday day mon_s yy hour min sec ->
        match month_of_string mon_s with
        | Some mon ->
          let year = if yy < 70 then 2000 + yy else 1900 + yy in
          to_epoch ~year ~mon ~day ~hour ~min ~sec
        | None -> None)
  with _ -> None

(* Parse any of the three HTTP-date forms into epoch seconds. *)
let parse (s : string) : float option =
  let s = String.trim s in
  match parse_imf s with
  | Some t -> Some t
  | None -> ( match parse_rfc850 s with Some t -> Some t | None -> parse_asctime s)

(* ──── parse ──── *)
let%test "parse IMF"            = parse "Sun, 06 Nov 1994 08:49:37 GMT" = Some 784_111_777.0
let%test "parse asctime"        = parse "Sun Nov  6 08:49:37 1994" = Some 784_111_777.0
let%test "parse rfc850"         = parse "Sunday, 06-Nov-94 08:49:37 GMT" = Some 784_111_777.0
let%test "parse epoch"          = parse "Thu, 01 Jan 1970 00:00:00 GMT" = Some 0.0
let%test "parse garbage"        = parse "not a date" = None
let%test "parse empty"          = parse "" = None
let%test "parse partial"        = parse "Sun, 06 Nov" = None
let%test "parse bad month"      = parse "Sun, 06 Xyz 1994 08:49:37 GMT" = None
let%test "parse ws tolerant"    = parse "  Sun, 06 Nov 1994 08:49:37 GMT  " = Some 784_111_777.0

(* ──── round-trip ──── *)
let%test "rt 0"                 = parse (format 0.0) = Some 0.0
let%test "rt 1"                 = parse (format 1.0) = Some 1.0
let%test "rt 784111777"         = parse (format 784_111_777.0) = Some 784_111_777.0
let%test "rt 1700000000"        = parse (format 1_700_000_000.0) = Some 1_700_000_000.0
let%test "rt 2000000000"        = parse (format 2_000_000_000.0) = Some 2_000_000_000.0
let%test "fractional truncates" = parse (format 100.9) = Some 100.0
