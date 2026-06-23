(* See engine.mli. The typed wrapper over the native FFI, plus the file I/O the CLI needs. The native
   call raises [Failure] on error (the buildkit seam); we turn that into a [result] here so the rest of
   the library and the CLI stay exception-free. *)

let process ~input ~format ~op =
  try Ok (Fennec_buildkit.Image.process ~input ~format:(Format.to_engine format) ~opts:(Op.to_opts op))
  with Failure m -> Error m

let read_file path =
  try Ok (In_channel.with_open_bin path (fun ic -> Bytes.of_string (In_channel.input_all ic)))
  with Sys_error m -> Error m

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let write_file path bytes =
  try
    mkdir_p (Filename.dirname path);
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_bytes oc bytes);
    Ok ()
  with
  | Sys_error m -> Error m
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let process_file ~input ~output ~format ~op =
  Result.bind (read_file input) (fun bytes ->
      Result.bind (process ~input:bytes ~format ~op) (fun out -> write_file output out))
