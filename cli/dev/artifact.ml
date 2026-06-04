(* See artifact.mli. *)

(* the OCaml EXECUTABLE bytecode trailer is "Caml1999X<nnn>"; the 'X' is the exec discriminant.
   Checking it (not just "Caml1999") rejects a .cmo/.cma ("Caml1999O"/"Caml1999A"/…) should [path]
   ever point at the wrong artifact, as well as a half-written file lacking the trailer. *)
let magic_prefix = "Caml1999X"

let bytecode_ready path =
  match (try Some (open_in_bin path) with _ -> None) with
  | None -> false
  | Some ic ->
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        len >= 12
        &&
        (seek_in ic (len - 12);
         let s = really_input_string ic 12 in
         String.length s = 12 && String.sub s 0 (String.length magic_prefix) = magic_prefix))
