(* See target.mli. Resolve the deployable from the same discovery the dev loop uses, then derive the
   NATIVE artifact paths under _build/default (release shares that build root — see
   {!Fennec_dev.Build_dir.release_context_dir}). [Discover] hands us the dev bytecode exe; for a release
   we want [<src_dir>/<name>.exe], the optimized native build, which the embed/webroot rules hang off
   exactly like the .bc does. *)

module Discover = Fennec_dev.Discover
module Build_dir = Fennec_dev.Build_dir

type t = {
  root : string;
  name : string;
  src_dir : string;
  exe_target : string;
  built_exe : string;
  assets_ml : string;
  webroot : string;
}

(* the pure path construction, split from {!resolve} so it is unit-testable without a filesystem: a bug
   here (wrong [_build] root, wrong target name) would silently aim the whole pipeline at a stale or
   absent artifact. [built_exe]/[assets_ml]/[webroot] live in the release build context (= _build/default
   per the build ladder; see {!Fennec_dev.Build_dir.release_context_dir}). *)
let paths ~root ~name ~src_dir : t =
  let in_src leaf = Filename.concat (Filename.concat (Build_dir.release_context_dir ~root) src_dir) leaf in
  { root;
    name;
    src_dir;
    exe_target = Filename.concat src_dir (name ^ ".exe");
    built_exe = in_src (name ^ ".exe");
    assets_ml = in_src "assets.ml";
    webroot = in_src "webroot" }

let resolve () : (t, string) result =
  match Discover.find () with
  | Error _ as e -> e
  | Ok d -> Ok (paths ~root:d.Discover.root ~name:d.Discover.name ~src_dir:d.Discover.src_dir)

(* ──── tests (the pure path layer) ──── *)

let%test "exe_target is the workspace-relative native target" =
  (paths ~root:"/ws" ~name:"server" ~src_dir:"examples/site").exe_target = "examples/site/server.exe"

let%test "built_exe lands under _build/default (release shares that context)" =
  (paths ~root:"/ws" ~name:"server" ~src_dir:"app").built_exe = "/ws/_build/default/app/server.exe"

let%test "assets_ml and webroot sit beside the built exe" =
  let t = paths ~root:"/ws" ~name:"s" ~src_dir:"a/b" in
  t.assets_ml = "/ws/_build/default/a/b/assets.ml" && t.webroot = "/ws/_build/default/a/b/webroot"
