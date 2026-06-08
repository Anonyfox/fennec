(* Configure-time discovery for the native mongo driver. Two jobs:

   1. Ensure a *from-source* build of mongo-c-driver exists in a persistent cache (vendor/build.sh
      does the work and is idempotent — only the first configure is slow). The driver is built as
      static archives with a *native* TLS backend (Secure Transport on macOS, OpenSSL on Linux) so
      the final binary trusts the OS certificate store and needs no pre-installed TLS libraries.

   2. Turn that cache prefix into linker/compiler flags via the driver's own *static* pkg-config
      files (mongoc2-static). Those carry the full .a paths plus the system libs each backend needs;
      the one transformation is rewriting any remaining dynamic `-l<name>` whose `lib<name>.a` we can
      find into that archive's full path — a full .a path forces static inclusion on both macOS ld64
      and GNU ld/lld, with no -Bstatic/-Bdynamic juggling. Genuinely OS-provided libs (resolv, the
      macOS frameworks, libc) have no archive and stay dynamic on purpose.

   Unlike comet (where the driver was mandatory), mongo is OPTIONAL in fennec, so any failure here —
   no cmake/curl, an unsupported OS (Windows, for now), a build error — DEGRADES instead of breaking
   the build: we emit [-DHAVE_MONGOC=0] and no link libs, the C stubs compile to functions that
   raise a clear error, and the :memory: backend keeps working. So `dune build` survives the whole
   matrix; where the build succeeds you get the self-contained, statically-linked driver. *)

module C = Configurator.V1

(* Run a command, returning trimmed stdout on success; never raises. *)
let capture c prog args =
  match try Some (C.Process.run c prog args) with _ -> None with
  | Some r when r.C.Process.exit_code = 0 -> ( match String.trim r.C.Process.stdout with "" -> None | s -> Some s)
  | _ -> None

let words = function None -> [] | Some s -> String.split_on_char ' ' s |> List.filter (fun w -> w <> "")

let dedup l =
  let seen = Hashtbl.create 16 in
  List.filter (fun x -> if Hashtbl.mem seen x then false else (Hashtbl.add seen x (); true)) l

let () =
  C.main ~name:"fennec_mongo" (fun c ->
      let build_sh =
        match Sys.getenv_opt "FENNEC_MONGO_BUILD_SH" with
        | Some p when p <> "" -> p
        | _ -> Filename.concat (Filename.dirname Sys.argv.(0)) "../vendor/build.sh"
      in
      (* try to build/locate the static driver and assemble flags; ANY failure → degrade *)
      let resolved =
        try
          let prefix =
            match capture c "bash" [ build_sh ] with
            | Some out -> ( match List.rev (String.split_on_char '\n' out) with last :: _ -> String.trim last | [] -> raise Exit)
            | None -> raise Exit
          in
          let pc_dir = Filename.concat prefix "lib/pkgconfig" in
          let pkg_config args =
            let arg_str = String.concat " " (List.map Filename.quote args) in
            capture c "sh" [ "-c"; Printf.sprintf "PKG_CONFIG_PATH=%s pkg-config %s" (Filename.quote pc_dir) arg_str ]
          in
          let cflags = words (pkg_config [ "--cflags"; "mongoc2-static" ]) in
          let raw_libs = words (pkg_config [ "--libs"; "--static"; "mongoc2-static" ]) in
          if cflags = [] && raw_libs = [] then raise Exit (* pkg-config found nothing → degrade *);
          (* -L dirs from the link line + the cache lib dir + standard system dirs: where we hunt for
             static archives to replace dynamic -l (OpenSSL's libssl.a/libcrypto.a on Linux live in a
             multiarch path the .pc never -L's, so without these they'd stay dynamic). *)
          let lib_dirs =
            dedup
              ((Filename.concat prefix "lib"
                :: List.filter_map
                     (fun s -> if String.length s > 2 && String.sub s 0 2 = "-L" then Some (String.sub s 2 (String.length s - 2)) else None)
                     raw_libs)
              @ [ "/usr/lib"; "/usr/local/lib"; "/usr/lib/x86_64-linux-gnu"; "/usr/lib/aarch64-linux-gnu" ])
            |> List.filter (fun d -> try Sys.is_directory d with _ -> false)
          in
          let archive_of name =
            let fname = "lib" ^ name ^ ".a" in
            List.find_map (fun d -> let p = Filename.concat d fname in if Sys.file_exists p then Some p else None) lib_dirs
          in
          (* the .pc spells the framework "Corefoundation"; the bundle is "CoreFoundation" — breaks
             on case-sensitive volumes. Normalize known framework names. *)
          let fix_framework tok =
            match String.lowercase_ascii tok with "corefoundation" -> "CoreFoundation" | "security" -> "Security" | _ -> tok
          in
          let libs =
            List.concat_map
              (fun tok ->
                if String.length tok > 2 && String.sub tok 0 2 = "-l" then
                  let name = String.sub tok 2 (String.length tok - 2) in
                  match archive_of name with Some a -> [ a ] | None -> [ tok ]
                else [ fix_framework tok ])
              raw_libs
          in
          Some (cflags, libs)
        with _ -> None
      in
      match resolved with
      | Some (cflags, libs) ->
          C.Flags.write_sexp "c_flags.sexp" ("-DHAVE_MONGOC=1" :: cflags);
          C.Flags.write_sexp "c_library_flags.sexp" libs
      | None ->
          prerr_endline
            "fennec-mongo: native driver disabled — libmongoc could not be built (no cmake/curl, an \
             unsupported OS, or a build error). The in-memory (:memory:) backend still works.";
          C.Flags.write_sexp "c_flags.sexp" [ "-DHAVE_MONGOC=0" ];
          C.Flags.write_sexp "c_library_flags.sexp" [])
