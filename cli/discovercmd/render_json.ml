open Discover_model

let item i = Snapshot.item_to_json i
let evidence e = Snapshot.evidence_to_json e

let confidence c = `String (confidence_to_string c)

let answer a =
  `Assoc
    [
      ("summary", `String a.summary);
      ("why", `List (List.map (fun s -> `String s) a.why));
      ("starter", match a.starter with Some s -> `String s | None -> `Null);
      ("copy_next", `List (List.map (fun s -> `String s) a.copy_next));
    ]

let render card =
  let json =
    match card with
    | Plan { task; answer = a; steps; uses; evidence = ev; avoid; confidence = c; reason; next } ->
      `Assoc
        [
          ("schema_version", `Int 1);
          ("card", `String "plan");
          ("task", `String task);
          ("answer", answer a);
          ("steps", `List (List.map (fun s -> `String s) steps));
          ("uses", `List (List.map item uses));
          ("evidence", `List (List.map evidence ev));
          ("avoid", `List (List.map (fun s -> `String s) avoid));
          ("confidence", confidence c);
          ("reason", `String reason);
          ("next", `List (List.map (fun s -> `String s) next));
        ]
    | Compare { task; answer = a; left; right; axis; left_when; right_when; evidence = ev; confidence = c; next } ->
      `Assoc
        [
          ("schema_version", `Int 1);
          ("card", `String "compare");
          ("task", `String task);
          ("answer", answer a);
          ("axis", `String axis);
          ("left", item left);
          ("right", item right);
          ("left_when", `String left_when);
          ("right_when", `String right_when);
          ("evidence", `List (List.map evidence ev));
          ("confidence", confidence c);
          ("next", `List (List.map (fun s -> `String s) next));
        ]
    | Browse { module_path; summary; items; evidence = ev; next } ->
      `Assoc
        [
          ("schema_version", `Int 1);
          ("card", `String "browse");
          ("module", `String module_path);
          ("summary", `String summary);
          ("items", `List (List.map item items));
          ("evidence", `List (List.map evidence ev));
          ("next", `List (List.map (fun s -> `String s) next));
        ]
    | Why { id; title; body; source; next } ->
      `Assoc
        [
          ("schema_version", `Int 1);
          ("card", `String "why");
          ("id", `String id);
          ("title", `String title);
          ("body", `List (List.map (fun s -> `String s) body));
          ("source", match source with Some s -> Source_ref.to_yojson s | None -> `Null);
          ("next", `List (List.map (fun s -> `String s) next));
        ]
    | Insufficient { task; reason; suggestions; inspect } ->
      `Assoc
        [
          ("schema_version", `Int 1);
          ("card", `String "insufficient");
          ("task", `String task);
          ("reason", `String reason);
          ("suggestions", `List (List.map (fun s -> `String s) suggestions));
          ( "inspect",
            `List
              (List.map
                 (fun c ->
                   `Assoc
                     [
                       ("id", `String c.id);
                       ("label", `String c.label);
                       ("source", Source_ref.to_yojson c.source);
                     ])
                 inspect) );
        ]
  in
  Yojson.Safe.to_string json
