(* Inline unit tests — the third hunt layer.

   Mirrors Http's output style (same caps/color/glyph, same check-per-line format) so the
   three layers look and feel like one tool. The registry is a global mutable list; tests
   register as module-init side effects and run in registration order (reversed at run-time
   since prepend is O(1)). Thread-safety is not required — one runner per library, sequential. *)

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Capabilities (self-contained — no dep on the main hunt library)                       *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

let getenv k = match Sys.getenv_opt k with Some "" | None -> None | s -> s
let truthy = function Some v -> v <> "0" && String.lowercase_ascii v <> "false" | None -> false

type caps = { color : bool; unicode : bool }

let detect_caps () : caps =
  let isatty = try Unix.isatty Unix.stdout with _ -> false in
  let term = Option.value ~default:"" (getenv "TERM") in
  let dumb = term = "dumb" in
  let color =
    if getenv "NO_COLOR" <> None then false
    else if truthy (getenv "FORCE_COLOR") || truthy (getenv "CLICOLOR_FORCE") then true
    else isatty && not dumb && term <> ""
  in
  let lc = Option.value ~default:"" (getenv "LC_ALL") ^ Option.value ~default:"" (getenv "LC_CTYPE") ^ Option.value ~default:"" (getenv "LANG") in
  let has_sub hay needle = let hl = String.length hay and nl = String.length needle in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in nl = 0 || go 0 in
  let unicode = (not dumb) && (not (truthy (getenv "FENNEC_HUNT_ASCII"))) && has_sub (String.lowercase_ascii lc) "utf" in
  { color; unicode }

let caps = lazy (detect_caps ())
let color code s = if (Lazy.force caps).color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let glyph uni ascii = if (Lazy.force caps).unicode then uni else ascii

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Registration                                                                          *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

type entry = {
  name : string;
  file : string;  (* "" when registered without location *)
  line : int;     (* 0 when unknown *)
  body : unit -> unit;  (* raises on failure *)
}

let registered : entry list ref = ref []

let add e = registered := e :: !registered

let test name body =
  add { name; file = ""; line = 0; body = (fun () -> if not (body ()) then failwith ("test failed: " ^ name)) }

let test_unit name body =
  add { name; file = ""; line = 0; body }

let test_loc ~name ~file ~line body =
  add { name; file; line; body = (fun () -> if not (body ()) then failwith ("test failed: " ^ name)) }

let test_unit_loc ~name ~file ~line body =
  add { name; file; line; body }

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Assertion helpers (for use inside test bodies)                                         *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

exception Check_failed of string

let check name cond =
  if not cond then raise (Check_failed (Printf.sprintf "check failed: %s" name))

let check_eq name ~expected ~got =
  if expected <> got then
    raise (Check_failed (Printf.sprintf "%s\n     expected: %s\n     got:      %s" name expected got))

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Test helpers                                                                          *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

(* substring search — self-contained, no fennec-hunt dep *)
let contains hay needle =
  let hl = String.length hay and nl = String.length needle in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let str_contains = contains

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  --grep filter (same semantics as Http and Browser: substring on the test name)         *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

let grep =
  lazy
    (let rec scan = function
       | "--grep" :: v :: _ -> Some v
       | _ :: r -> scan r
       | [] -> None
     in
     match Array.to_list Sys.argv with _ :: rest -> scan rest | [] -> None)

let selected name = match Lazy.force grep with None -> true | Some g -> contains name g

(* ═══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Execution                                                                             *)
(* ═══════════════════════════════════════════════════════════════════════════════════════ *)

let count () = List.length !registered

let loc_str e =
  if e.file = "" then "" else Printf.sprintf " %s(%s:%d)%s" (color "2" "") e.file e.line (color "0" "")

let run () =
  let tests = List.rev !registered in
  let n = List.length tests in
  let passed = ref 0 and failed = ref 0 and skipped = ref 0 in
  List.iter (fun (e : entry) ->
    if not (selected e.name) then (
      incr skipped;
      Printf.printf "  %s  %s\n%!" (color "2" (glyph "–" "-")) (color "2" (e.name ^ " (skipped)")))
    else begin
      let t0 = Unix.gettimeofday () in
      match e.body () with
      | () ->
        let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        incr passed;
        Printf.printf "  %s  %s %s%s\n%!" (color "32" (glyph "✓" "ok")) e.name
          (color "2" (Printf.sprintf "(%.0fms)" ms)) (loc_str e)
      | exception (Check_failed msg) ->
        let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        incr failed;
        Printf.printf "  %s  %s %s%s\n%!" (color "31" (glyph "✗" "FAIL")) e.name
          (color "2" (Printf.sprintf "(%.0fms)" ms)) (loc_str e);
        String.split_on_char '\n' msg |> List.iter (fun line -> Printf.printf "     %s\n%!" line)
      | exception exn ->
        let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        incr failed;
        Printf.printf "  %s  %s %s%s\n%!" (color "31" (glyph "✗" "FAIL")) e.name
          (color "2" (Printf.sprintf "(%.0fms)" ms)) (loc_str e);
        let detail = match exn with Failure m -> m | other -> Printexc.to_string other in
        String.split_on_char '\n' detail |> List.iter (fun line -> Printf.printf "     %s\n%!" line)
    end)
    tests;
  let skip_note = if !skipped > 0 then Printf.sprintf " (%d skipped)" !skipped else "" in
  Printf.printf "\n";
  if !failed > 0 then
    Printf.printf "  %s%s\n%!" (color "31" (Printf.sprintf "%d of %d tests failed" !failed n)) (color "2" skip_note)
  else
    Printf.printf "  %s%s\n%!" (color "32" (Printf.sprintf "%d tests passed" !passed)) (color "2" skip_note);
  if !failed > 0 then 1 else 0
