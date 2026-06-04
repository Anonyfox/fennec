(* See artifact.mli. *)

let magic_prefix = "Caml1999" (* the OCaml bytecode trailer is "Caml1999X<nnn>" *)

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
