(* GLOBAL state = signals in a shared module. (Iso is auto-opened via -open Iso.) *)
type todo = { id : int; text : string }
let todos : todo list signal = signal []
let next_id = signal 1
let add text =
  let id = peek next_id in
  set next_id (id + 1);
  update todos (fun l -> l @ [ { id; text } ])
let remove id = update todos (List.filter (fun t -> t.id <> id))
