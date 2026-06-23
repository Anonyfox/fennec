(* See stage.mli. Copy the artifact out of _build/ and strip it.

   The copy is chunked (the binary is tens of MB) and follows dune's symlink to the real file. We then
   chmod 0o755 (dune outputs are read-only) and `strip` in place. strip is best-effort: on macOS it
   mostly no-ops (debug info lives in a separate .dSYM that never ships); on Linux it drops the DWARF
   the OCaml/Rust/Go link leaves behind. If `strip` is missing or fails, the unstripped binary is still
   a valid deployable, so we warn and carry on rather than fail the release. *)

let copy src dst =
  In_channel.with_open_bin src (fun ic ->
      Out_channel.with_open_bin dst (fun oc ->
          let buf = Bytes.create 65536 in
          let rec loop () =
            let n = In_channel.input ic buf 0 (Bytes.length buf) in
            if n > 0 then (Out_channel.output oc buf 0 n; loop ())
          in
          loop ()))

type staged = {
  path : string;
  bytes : int;
}

let run ~built_exe ~outdir ~name ~strip : (staged, string) result =
  if not (Sys.file_exists built_exe) then Error (Printf.sprintf "built binary not found at %s" built_exe)
  else
    match
      Util.mkdir_p outdir;
      let path = Filename.concat outdir name in
      copy built_exe path;
      Unix.chmod path 0o755;
      path
    with
    | exception Sys_error msg -> Error msg
    | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
    | path ->
      if strip then begin
        let rc = Sys.command (Printf.sprintf "strip %s 2>/dev/null" (Filename.quote path)) in
        if rc <> 0 then Printf.eprintf "fennec release: note — `strip` unavailable or failed; shipping the unstripped binary.\n%!"
      end;
      let bytes = (try (Unix.stat path).Unix.st_size with _ -> 0) in
      Ok { path; bytes }
