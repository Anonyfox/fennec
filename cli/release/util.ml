(* See util.mli. The few filesystem primitives the pipeline phases share. Kept here (rather than reusing
   cli/webroot.ml's copies) because webroot.ml is part of the `fennec` executable, not a library. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let rec count_files dir =
  match Sys.readdir dir with
  | exception _ -> 0
  | entries ->
    Array.fold_left
      (fun acc name ->
        let p = Filename.concat dir name in
        if (try Sys.is_directory p with _ -> false) then acc + count_files p else acc + 1)
      0 entries
