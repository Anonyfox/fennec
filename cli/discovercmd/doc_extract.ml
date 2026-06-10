open Ppxlib
open Discover_model

let doc_text attrs =
  List.find_map
    (fun (a : attribute) ->
      if a.attr_name.txt <> "ocaml.doc" then None
      else
        match a.attr_payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
                _;
              };
            ] ->
          Some (String.trim s)
        | _ -> None)
    attrs

let line loc = loc.Location.loc_start.pos_lnum

let type_string ty = Format.asprintf "%a" Pprintast.core_type ty

let value_signature (vd : value_description) =
  Some (Printf.sprintf "val %s : %s" vd.pval_name.txt (type_string vd.pval_type))

let module_type_summary = function
  | Pmty_signature _ -> Some "module : sig ..."
  | Pmty_ident lid -> Some (Format.asprintf "module : %a" Pprintast.longident lid.txt)
  | _ -> Some "module"

let rec signature_items ~package ~library ~root ~path file (sg : signature) =
  List.concat_map
    (fun (it : signature_item) ->
      match it.psig_desc with
      | Psig_value vd ->
        let full = String.concat "." (root :: path @ [ vd.pval_name.txt ]) in
        [
          {
            id = "api:" ^ full;
            package;
            library;
            path = full;
            kind = Value;
            signature = value_signature vd;
            doc = doc_text vd.pval_attributes;
            source = Source_ref.make ~path:file ~line:(line vd.pval_loc) ();
          };
        ]
      | Psig_type (_, tds) | Psig_typesubst tds ->
        List.map
          (fun td ->
            let full = String.concat "." (root :: path @ [ td.ptype_name.txt ]) in
            {
              id = "api:" ^ full;
              package;
              library;
              path = full;
              kind = Type;
              signature = Some (Printf.sprintf "type %s" td.ptype_name.txt);
              doc = doc_text td.ptype_attributes;
              source = Source_ref.make ~path:file ~line:(line td.ptype_loc) ();
            })
          tds
      | Psig_exception te ->
        let full =
          String.concat "." (root :: path @ [ te.ptyexn_constructor.pext_name.txt ])
        in
        let doc =
          match doc_text te.ptyexn_constructor.pext_attributes with
          | Some _ as d -> d
          | None -> doc_text te.ptyexn_attributes
        in
        [
          {
            id = "api:" ^ full;
            package;
            library;
            path = full;
            kind = Exception;
            signature = Some ("exception " ^ te.ptyexn_constructor.pext_name.txt);
            doc;
            source = Source_ref.make ~path:file ~line:(line te.ptyexn_loc) ();
          };
        ]
      | Psig_module md ->
        let name = Option.value md.pmd_name.txt ~default:"_" in
        let full = String.concat "." (root :: path @ [ name ]) in
        let self =
          {
            id = "api:" ^ full;
            package;
            library;
            path = full;
            kind = Module;
            signature = module_type_summary md.pmd_type.pmty_desc;
            doc = doc_text md.pmd_attributes;
            source = Source_ref.make ~path:file ~line:(line md.pmd_loc) ();
          }
        in
        let nested =
          match md.pmd_type.pmty_desc with
          | Pmty_signature nested -> signature_items ~package ~library ~root ~path:(path @ [ name ]) file nested
          | _ -> []
        in
        self :: nested
      | Psig_modtype mtd ->
        let full = String.concat "." (root :: path @ [ mtd.pmtd_name.txt ]) in
        [
          {
            id = "api:" ^ full;
            package;
            library;
            path = full;
            kind = Module_type;
            signature = Some "module type";
            doc = doc_text mtd.pmtd_attributes;
            source = Source_ref.make ~path:file ~line:(line mtd.pmtd_loc) ();
          };
        ]
      | _ -> [])
    sg

let parse_interface ~package ~library ~root ~file ~contents =
  let lexbuf = Lexing.from_string contents in
  Lexing.set_filename lexbuf file;
  try signature_items ~package ~library ~root ~path:[] file (Parse.interface lexbuf) with _ -> []

let root_module_of_file file =
  Filename.basename file |> Filename.remove_extension |> String.capitalize_ascii

let%test "extracts nested facade values" =
  let src = "module Paw : sig\n  module Basic_auth : sig\n    val make : unit -> int\n  end\nend\n" in
  match parse_interface ~package:"fennec" ~library:"fennec" ~root:"Fennec" ~file:"x.mli" ~contents:src with
  | items -> List.exists (fun i -> i.path = "Fennec.Paw.Basic_auth.make") items
