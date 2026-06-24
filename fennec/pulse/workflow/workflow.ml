(* Workflows are ordinary functions over ambient data + effects. A [Workflow.t] adds the two things a
   plain function lacks for the non-web core: it runs inside the transparent transaction (commit on
   return, roll back on raise) and it carries its reactions. Reactions live on the value itself — no
   global stringly registry — so [@after Orders.place] in another module is a normal, type-checked
   cross-module reference to the workflow value. The reactions ppx turns [@before f] / [@after f]
   annotations into {!before} / {!after} calls; this module is what they call. *)

module Tx = Fennec_mongo_dynamic.Tx

type ('a, 'r) t = {
  name : string;
  fn : 'a -> 'r;
  mutable befores : ('a -> unit) list; (* in-transaction guards — may raise to veto *)
  mutable afters : ('r -> unit) list; (* post-commit reactions — isolated *)
}

let make name fn = { name; fn; befores = []; afters = [] }
let name t = t.name
let fn t = t.fn

(* append (not prepend) so reactions fire in declaration order *)
let before t h = t.befores <- t.befores @ [ h ]
let after t h = t.afters <- t.afters @ [ h ]

(* an after-hook is isolated: a failing reaction must not break the workflow result or its siblings,
   but it is logged so a swallowed failure is still visible *)
let safe_after t r h =
  try h r
  with
  | (Stack_overflow | Out_of_memory) as e -> raise e
  | e -> Printf.eprintf "fennec: reaction after %s raised: %s\n%!" t.name (Printexc.to_string e)

let call t x =
  let r =
    Tx.run (fun () ->
        List.iter (fun guard -> guard x) t.befores; (* in the transaction: a raise vetoes + rolls back *)
        t.fn x)
  in
  (* committed — defer each after-hook to the OUTERMOST commit (so a nested workflow's reactions fire
     once the whole transaction is durable), each isolated *)
  List.iter (fun h -> Tx.on_commit (fun () -> safe_after t r h)) t.afters;
  r
