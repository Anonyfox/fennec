(* Pluggable storage for the ACME account key + issued certificates. The right backing genuinely
   depends on the deployment: a writable volume (VM / docker volume / k8s PVC) → {!file}; an
   ephemeral or multi-replica deployment → an external shared store (the app implements {!t} over a
   k8s Secret / S3 / Redis / a DB) so a restart doesn't re-issue and replicas share one cert. The
   framework ships [file] (the default) + [memory] (dev/test) and exposes the seam.

   Keys are opaque namespaced strings; values are PEM or JSON text. [with_lease] is the multi-
   instance dedup primitive — only the holder runs the thunk, so N replicas don't all order certs at
   once (Let's Encrypt's duplicate-cert rate limit is low). *)

type t = {
  get : string -> string option;
  put : string -> string -> unit;
  delete : string -> unit;
  with_lease : string -> (unit -> unit) -> bool;
      (* run the thunk holding a lease on [key]; [false] if another holder has it. Single-process
         stores always grant it; a shared store implements a real distributed lock. *)
}

(* ---- memory: dev / test / ephemeral (lost on restart → re-issues; fine for those) ---- *)
let memory () : t =
  let tbl : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let mu = Mutex.create () in
  let locked f = Mutex.lock mu; Fun.protect ~finally:(fun () -> Mutex.unlock mu) f in
  {
    get = (fun k -> locked (fun () -> Hashtbl.find_opt tbl k));
    put = (fun k v -> locked (fun () -> Hashtbl.replace tbl k v));
    delete = (fun k -> locked (fun () -> Hashtbl.remove tbl k));
    with_lease = (fun _ f -> f (); true) (* single process: always granted *);
  }

(* ---- file: the default for a persistent volume. Atomic temp+rename, 0600, under [dir] ---- *)
let file ~dir : t =
  (try if not (Sys.file_exists dir) then Unix.mkdir dir 0o700 with _ -> ());
  (* a key maps to a filename with separators flattened, so it can never escape [dir] *)
  let path k = Filename.concat dir (String.map (fun c -> if c = '/' || c = '\\' || c = Filename.dir_sep.[0] then '_' else c) k) in
  let get k = let p = path k in if Sys.file_exists p then Some (In_channel.with_open_bin p In_channel.input_all) else None in
  let put k v =
    let p = path k in
    let tmp = p ^ ".tmp" in
    Out_channel.with_open_bin tmp (fun oc -> output_string oc v);
    (try Unix.chmod tmp 0o600 with _ -> ());
    Sys.rename tmp p (* atomic within the dir *)
  in
  let delete k = try Sys.remove (path k) with _ -> () in
  (* a coarse cross-process lease via an O_EXCL lockfile (good enough on a shared POSIX volume); the
     thunk runs iff we created the lockfile, and we always remove it afterwards *)
  let with_lease k f =
    let lock = path (k ^ ".lock") in
    match (try Some (Unix.openfile lock [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY ] 0o600) with Unix.Unix_error (Unix.EEXIST, _, _) -> None) with
    | None -> false
    | Some fd -> Fun.protect ~finally:(fun () -> (try Unix.close fd with _ -> ()); (try Sys.remove lock with _ -> ())) (fun () -> f (); true)
  in
  { get; put; delete; with_lease }

(* ──── cert_store tests ──── *)

let%test "memory: put/get/delete round-trip + lease grants" =
  let s = memory () in
  s.put "a" "1";
  let ran = ref false in
  let granted = s.with_lease "a" (fun () -> ran := true) in
  s.get "a" = Some "1" && (s.delete "a"; s.get "a" = None) && granted && !ran

let%test "file: persists atomically + a held lease blocks a second acquire" =
  let dir = Filename.temp_dir "fennec_cs_" "" in
  Fun.protect
    ~finally:(fun () -> (try Sys.rmdir dir with _ -> ()))
    (fun () ->
      let s = file ~dir in
      s.put "acct/key" "PEM";
      let ok = s.get "acct/key" = Some "PEM" in
      (* while a lease is held, a nested acquire of the same key is refused *)
      let nested_refused = ref false in
      let outer = s.with_lease "acct/key" (fun () -> nested_refused := not (s.with_lease "acct/key" (fun () -> ()))) in
      s.delete "acct/key";
      ok && outer && !nested_refused && s.get "acct/key" = None)
