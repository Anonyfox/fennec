(* Dogfood for `let%prop` — proves the property cut end-to-end through the real pipeline:
   the ppx expands each form, registers it in the inline Unit runtime, and the runner executes
   it (each property = many generated cases via QCheck2). Mirrors test_unit.ml for the unit cut.

   Covers every authoring shape so a regression in the ppx (type→generator+printer derivation,
   currying adapter, the explicit forall passthrough) shows up as a failed `dune runtest`. *)

open Fennec_hunt.Prop

(* ── type-driven: single argument (generator + printer both derived from `int list`) ── *)
let%prop "reversing a list twice is the identity" = fun (l : int list) ->
  List.rev (List.rev l) = l

let%prop "sorting is idempotent" = fun (l : int list) ->
  List.sort compare (List.sort compare l) = List.sort compare l

let%prop "string length is non-negative" = fun (s : string) ->
  String.length s >= 0

(* ── type-driven: multiple arguments (curried; tupled by the uncurry adapter) ── *)
let%prop "list append length is additive" = fun (a : int list) (b : int list) ->
  List.length (a @ b) = List.length a + List.length b

let%prop "string concat length is additive" = fun (a : string) (b : string) ->
  String.length (a ^ b) = String.length a + String.length b

(* ── type-driven: nested type (option of a pair) ── *)
let%prop "option-pair round-trips through a match" = fun (o : (int * bool) option) ->
  (match o with None -> true | Some (n, b) -> (n = n) && (b || not b))

(* ── precondition with assume (discarded cases don't count as failures) ── *)
let%prop "div/mod reconstruct the dividend" = fun (a : int) (b : int) ->
  assume (b <> 0);
  a / b * b + a mod b = a

(* ── explicit form: a custom generator a type can't express (a bounded range) ── *)
let%prop "a value in [0,100] clamps into [10,90]" =
  forall ~print:Print.int Gen.(int_range 0 100) (fun n ->
    let c = max 10 (min 90 n) in
    c >= 10 && c <= 90)

let () = exit (Fennec_hunt_unit.run ())
