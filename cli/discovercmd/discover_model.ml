type confidence = High | Medium | Low | Conf_insufficient

let confidence_to_string = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"
  | Conf_insufficient -> "insufficient"

let confidence_of_string = function
  | "high" -> High
  | "medium" -> Medium
  | "low" -> Low
  | _ -> Conf_insufficient

type item_kind = Module | Value | Type | Exception | Module_type

let kind_to_string = function
  | Module -> "module"
  | Value -> "value"
  | Type -> "type"
  | Exception -> "exception"
  | Module_type -> "module type"

let kind_of_string = function
  | "module" -> Module
  | "value" | "val" | "let" | "external" -> Value
  | "type" -> Type
  | "exception" -> Exception
  | "module type" -> Module_type
  | _ -> Value

type public_item = {
  id : string;
  package : string;
  library : string;
  path : string;
  kind : item_kind;
  signature : string option;
  doc : string option;
  source : Source_ref.t;
}

type evidence_kind = Example | Test | Doctest | Route | Hazard

let evidence_kind_to_string = function
  | Example -> "example"
  | Test -> "test"
  | Doctest -> "doctest"
  | Route -> "route"
  | Hazard -> "hazard"

let evidence_kind_of_string = function
  | "example" -> Example
  | "test" -> Test
  | "doctest" -> Doctest
  | "route" -> Route
  | "hazard" -> Hazard
  | _ -> Example

type evidence = {
  id : string;
  kind : evidence_kind;
  package : string;
  label : string;
  text : string;
  apis : string list;
  source : Source_ref.t;
}

type package = {
  name : string;
  version : string;
  digest : string;
}

type posting = {
  term : string;
  refs : int array;
}

type snapshot = {
  schema_version : int;
  generated_at : string;
  packages : package list;
  public_items : public_item list;
  api_index : posting list;
  evidence : evidence list;
  evidence_index : posting list;
  api_evidence_index : posting list;
}

type source_claim = {
  id : string;
  label : string;
  source : Source_ref.t;
}

type answer = {
  summary : string;
  why : string list;
  starter : string option;
  copy_next : string list;
}

type card =
  | Plan of {
      task : string;
      answer : answer;
      steps : string list;
      uses : public_item list;
      evidence : evidence list;
      avoid : string list;
      confidence : confidence;
      reason : string;
      next : string list;
    }
  | Compare of {
      task : string;
      answer : answer;
      left : public_item;
      right : public_item;
      axis : string;
      left_when : string;
      right_when : string;
      evidence : evidence list;
      confidence : confidence;
      next : string list;
    }
  | Browse of {
      module_path : string;
      summary : string;
      items : public_item list;
      evidence : evidence list;
      next : string list;
    }
  | Why of {
      id : string;
      title : string;
      body : string list;
      source : Source_ref.t option;
      next : string list;
    }
  | Insufficient of {
      task : string;
      reason : string;
      suggestions : string list;
      inspect : source_claim list;
    }

type options = {
  json : bool;
  more : bool;
  query : string option;
  why : string option;
  browse : string option;
}
