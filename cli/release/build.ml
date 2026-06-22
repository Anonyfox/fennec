(* See build.mli. The one-shot release build.

   We shell out (rather than fork dune via the API) so dune's coloured progress + error frames stream
   straight to the user's terminal, and so a `cd <root>` makes the workspace-relative target resolve no
   matter the cwd — the same shape `fennec console` uses to build its target. `env -u DUNE_BUILD_DIR`
   strips any inherited fastdev build root (a live `fennec dev` exports one) so `--profile release`
   always lands in `_build/default`, where {!Target} looks for the artifact. *)

let run ~root ~target : (unit, string) result =
  let cmd =
    Printf.sprintf "cd %s && env -u DUNE_BUILD_DIR dune build --profile %s %s" (Filename.quote root)
      Fennec_dev.Build_dir.release_profile (Filename.quote target)
  in
  if Sys.command cmd = 0 then Ok ()
  else Error (Printf.sprintf "`dune build --profile %s %s` failed (see the output above)" Fennec_dev.Build_dir.release_profile target)
