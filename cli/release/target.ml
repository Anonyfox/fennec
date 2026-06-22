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

let resolve () : (t, string) result =
  match Discover.find () with
  | Error _ as e -> e
  | Ok d ->
    let root = d.Discover.root and name = d.Discover.name and src_dir = d.Discover.src_dir in
    let in_src leaf = Filename.concat (Filename.concat (Build_dir.release_context_dir ~root) src_dir) leaf in
    Ok
      { root;
        name;
        src_dir;
        exe_target = Filename.concat src_dir (name ^ ".exe");
        built_exe = in_src (name ^ ".exe");
        assets_ml = in_src "assets.ml";
        webroot = in_src "webroot" }
