(* The todo-input form model — the Sift codec behind the <Todo_list> draft input. It replaces the old
   hand-written "is it empty?" check with a declarative refinement catalog: [@trim] normalizes, [@non_empty]
   rejects a blank, [@max_len 80] caps the length — all enforced client-side, reactively, by [Form.signal]
   (web/components/todo_list.mlx) with zero round-trips. One model, [@@deriving model] makes the codec
   + the typed [Fields] handle; tighten a rule here and the form's validation AND its HTML5 input attrs
   move together. Plain shared model in the store so it is reachable from the component. *)

type t = { text : string [@trim] [@non_empty] [@max_len 80] }
[@@deriving model]

let empty = { text = "" }
