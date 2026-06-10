open Discover_model

let schema_version = 1

let package_to_json p =
  `Assoc [ ("name", `String p.name); ("version", `String p.version); ("digest", `String p.digest) ]

let package_of_json = function
  | `Assoc fields ->
    let s k = match List.assoc_opt k fields with Some (`String v) -> v | _ -> "" in
    { name = s "name"; version = s "version"; digest = s "digest" }
  | _ -> { name = ""; version = ""; digest = "" }

let posting_to_json p =
  `Assoc [ ("term", `String p.term); ("refs", `List (Array.fold_right (fun n acc -> `Int n :: acc) p.refs [])) ]

let posting_of_json = function
  | `Assoc fields ->
    {
      term = (match List.assoc_opt "term" fields with Some (`String s) -> s | _ -> "");
      refs =
        (match List.assoc_opt "refs" fields with
        | Some (`List xs) -> List.filter_map (function `Int n -> Some n | _ -> None) xs |> Array.of_list
        | _ -> [||]);
    }
  | _ -> { term = ""; refs = [||] }

let item_to_json (i : public_item) =
  `Assoc
    [
      ("id", `String i.id);
      ("package", `String i.package);
      ("library", `String i.library);
      ("path", `String i.path);
      ("kind", `String (kind_to_string i.kind));
      ("signature", match i.signature with Some s -> `String s | None -> `Null);
      ("doc", match i.doc with Some s -> `String s | None -> `Null);
      ("source", Source_ref.to_yojson i.source);
    ]

let item_of_json = function
  | `Assoc fields ->
    let s k = match List.assoc_opt k fields with Some (`String v) -> v | _ -> "" in
    let opt k = match List.assoc_opt k fields with Some (`String v) -> Some v | _ -> None in
    {
      id = s "id";
      package = s "package";
      library = s "library";
      path = s "path";
      kind = kind_of_string (s "kind");
      signature = opt "signature";
      doc = opt "doc";
      source = (match List.assoc_opt "source" fields with Some j -> Source_ref.of_yojson j | None -> Source_ref.make ~path:"" ~line:1 ());
    }
  | _ ->
    {
      id = "";
      package = "";
      library = "";
      path = "";
      kind = Value;
      signature = None;
      doc = None;
      source = Source_ref.make ~path:"" ~line:1 ();
    }

let evidence_to_json (e : evidence) =
  `Assoc
    [
      ("id", `String e.id);
      ("kind", `String (evidence_kind_to_string e.kind));
      ("package", `String e.package);
      ("label", `String e.label);
      ("text", `String e.text);
      ("apis", `List (List.map (fun s -> `String s) e.apis));
      ("source", Source_ref.to_yojson e.source);
    ]

let evidence_of_json = function
  | `Assoc fields ->
    let s k = match List.assoc_opt k fields with Some (`String v) -> v | _ -> "" in
    {
      id = s "id";
      kind = evidence_kind_of_string (s "kind");
      package = s "package";
      label = s "label";
      text = s "text";
      apis =
        (match List.assoc_opt "apis" fields with
        | Some (`List xs) -> List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []);
      source = (match List.assoc_opt "source" fields with Some j -> Source_ref.of_yojson j | None -> Source_ref.make ~path:"" ~line:1 ());
    }
  | _ ->
    {
      id = "";
      kind = Example;
      package = "";
      label = "";
      text = "";
      apis = [];
      source = Source_ref.make ~path:"" ~line:1 ();
    }

let to_json t =
  `Assoc
    [
      ("schema_version", `Int t.schema_version);
      ("generated_at", `String t.generated_at);
      ("packages", `List (List.map package_to_json t.packages));
      ("public_items", `List (List.map item_to_json t.public_items));
      ("api_index", `List (List.map posting_to_json t.api_index));
      ("evidence", `List (List.map evidence_to_json t.evidence));
      ("evidence_index", `List (List.map posting_to_json t.evidence_index));
      ("api_evidence_index", `List (List.map posting_to_json t.api_evidence_index));
    ]

let of_json = function
  | `Assoc fields ->
    {
      schema_version = (match List.assoc_opt "schema_version" fields with Some (`Int n) -> n | _ -> 0);
      generated_at = (match List.assoc_opt "generated_at" fields with Some (`String s) -> s | _ -> "");
      packages =
        (match List.assoc_opt "packages" fields with Some (`List xs) -> List.map package_of_json xs | _ -> []);
      public_items =
        (match List.assoc_opt "public_items" fields with Some (`List xs) -> List.map item_of_json xs | _ -> []);
      api_index =
        (match List.assoc_opt "api_index" fields with Some (`List xs) -> List.map posting_of_json xs | _ -> []);
      evidence =
        (match List.assoc_opt "evidence" fields with Some (`List xs) -> List.map evidence_of_json xs | _ -> []);
      evidence_index =
        (match List.assoc_opt "evidence_index" fields with Some (`List xs) -> List.map posting_of_json xs | _ -> []);
      api_evidence_index =
        (match List.assoc_opt "api_evidence_index" fields with Some (`List xs) -> List.map posting_of_json xs | _ -> []);
    }
  | _ -> { schema_version = 0; generated_at = ""; packages = []; public_items = []; api_index = []; evidence = []; evidence_index = []; api_evidence_index = [] }

let to_string t = Yojson.Safe.to_string (to_json t)
let of_string s = of_json (Yojson.Safe.from_string s)
