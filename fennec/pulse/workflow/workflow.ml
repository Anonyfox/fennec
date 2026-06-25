(* The workflow runtime — the machinery behind the [@workflow] ppx. Userland writes ORDINARY functions
   tagged [@workflow], and reactions tagged [@after f] / [@before f]; the ppx lowers each workflow to a
   call to {!exec} plus two typed registration lists, so the function still reads like a normal function
   and the transaction + reactions are transparent. You rarely touch this module directly. *)

module Tx = Fennec_mongo_dynamic.Tx

exception Cyclic_reaction of string

(* The runtime half of the circuit-breaker: a fiber-local stack of the workflows currently inside their
   own reaction cascade. (The compile-time half — the ppx + module dependency cycles — rejects cyclic
   [@after]/[@before] graphs; this catches a reaction whose BODY calls back into a workflow that
   cascades to it.) Fiber-local so concurrent requests don't see each other's stack, with the usual
   no-scheduler fallback for unit tests. *)
let _active_key : string list Eio.Fiber.key = Eio.Fiber.create_key ()
let _active_fallback = ref []

let active () =
  match (try Eio.Fiber.get _active_key with Stdlib.Effect.Unhandled _ -> None) with
  | Some l -> l
  | None -> !_active_fallback

let with_active name f =
  match (try `Eio (ignore (Eio.Fiber.get _active_key : string list option)) with Stdlib.Effect.Unhandled _ -> `Plain) with
  | `Eio () -> Eio.Fiber.with_binding _active_key (name :: active ()) f
  | `Plain ->
      let saved = !_active_fallback in
      _active_fallback := name :: saved;
      Fun.protect f ~finally:(fun () -> _active_fallback := saved)

(* an after-hook is isolated: a failing reaction must not break the workflow result or its siblings,
   but it is logged so a swallowed failure stays visible *)
let safe_after name r h =
  try h r
  with
  | (Stack_overflow | Out_of_memory) as e -> raise e
  | e -> Printf.eprintf "fennec: reaction after %s raised: %s\n%!" name (Printexc.to_string e)

(* [exec ~name ~befores ~afters arg body] is what a [@workflow] function compiles to: run the
   before-guards then the body in ONE transparent transaction (commit on return, roll back on raise,
   read-your-writes); on commit, fire the after-hooks with the RESULT — deferred to the outermost
   commit (so a nested workflow's reactions fire once the whole thing is durable) and isolated. The
   re-entrancy guard raises {!Cyclic_reaction} if this workflow is already in its own cascade. Fully
   generic + type-safe: [befores : ('a -> unit) list], [afters : ('r -> unit) list]. *)
let exec ~name ~befores ~afters arg body =
  if List.mem name (active ()) then raise (Cyclic_reaction name);
  with_active name (fun () ->
      let r = Tx.run (fun () -> List.iter (fun g -> g arg) befores; body ()) in
      List.iter (fun h -> Tx.on_commit (fun () -> safe_after name r h)) afters;
      r)
