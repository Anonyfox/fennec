(** multipart/form-data parsing (RFC 7578) — pure. Splits a request body into parts,
    each with its field name, optional filename (for uploads), content type, and raw
    payload.

    {[
      match Multipart.boundary_of_content_type content_type with
      | Some boundary ->
        let parts = Multipart.parse ~boundary body in
        let files = List.filter (fun (p : Multipart.part) -> p.filename <> None) parts in
        ignore files
      | None -> ()                                  (* not a multipart body *)
    ]} *)

(** One part of a [multipart/form-data] body: the form field name, an optional filename
    (for uploaded files), the part's content type, and its raw payload bytes. *)
type part = {
  name : string;             (** the form field name *)
  filename : string option;  (** present for file parts *)
  content_type : string;     (** the part's content type (default ["text/plain"]) *)
  data : string;             (** the raw payload bytes *)
}

(** The boundary token from a multipart [Content-Type] value, if present. *)
val boundary_of_content_type : string -> string option

(** Parse a multipart/form-data body given its boundary. *)
val parse : boundary:string -> string -> part list
