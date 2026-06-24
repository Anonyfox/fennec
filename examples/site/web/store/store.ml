(* GLOBAL state — signals declared at module scope are shared across every component
   and page that reads them (vs a signal created inside a component's setup, which is
   per-instance). Plain .ml — no JSX, no ppx. The Todo_list mutates it; Stats reads it;
   both stay in sync reactively, anywhere in the tree. *)
type todo = { id : int; text : string }

let todos : todo list signal = signal []
let next_id = signal 1

let add text =
  let id = peek next_id in
  set next_id (id + 1);
  update todos (fun l -> l @ [ { id; text } ])

let remove id = update todos (List.filter (fun t -> t.id <> id))
