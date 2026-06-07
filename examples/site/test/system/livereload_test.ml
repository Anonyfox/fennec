(* Livereload system test (dev-only), ported from livereload.sh. Runs the REAL `fennec dev`
   against the site example and guards the ways livereload has actually broken before —
   deterministically, no browser. Typed and contained (the System layer reaps the whole process
   group on teardown — no orphans between scenarios).

   Guards:
     1. SINGLE INSTANCE: a second `fennec dev` reaps the first (no stale supervisor wins the port).
     2. STARTUP FRESHNESS: the server serves the CURRENT on-disk source, not a stale _build.
     3. NO-CACHE: page + client bundle are served no-cache in dev.
     4. EDIT PROPAGATION: editing a frontend source reaches BOTH the SSR and the rebuilt bundle.
     5. REVERT PROPAGATION: undoing the edit (while dev is alive) leaves no stale _build. *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let getenv k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d
let fennec = getenv "FENNEC_BIN" "fennec"
let app_dir = getenv "FENNEC_APP_DIR" (Sys.getcwd ())

let replace s ~old ~by =
  let n = String.length old and b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i + n <= String.length s && String.sub s !i n = old then (Buffer.add_string b by; i := !i + n)
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

(* dev port model: gateway=4000, web endpoint=4001 *)
let page = 4001 and page_path = "/"
let bundle_path = "/_apps/web/main.js"
let src = Filename.concat app_dir "frontend/apps/web/index.mlx"
let disk = "Welcome to the Fennec site"
let mark = "LIVERELOAD_MARK_XYZ"

(* resilient probes: a request during a server restart raises Connection_refused — swallow it and
   report "not yet", so [wait_until] keeps polling instead of failing on a transient window *)
let body_has port path needle = try contains (S.request port path).S.body needle with _ -> false
let cache_no_cache port path =
  try match S.header (S.request port path) "cache-control" with Some v -> contains (String.lowercase_ascii v) "no-cache" | None -> false
  with _ -> false

let () = S.main @@ fun () ->

  S.test "single instance, startup freshness, no-cache, edit+revert propagation" (fun sb ->
    let d1 = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_ready d1 ~port:page ();

    (* 1) single instance — a second start reaps the first (kept alive by the sandbox switch) *)
    let _d2 = S.spawn sb ~cwd:app_dir [ fennec; "dev" ] in
    S.wait_until ~timeout:15.0 (fun () -> not (S.alive d1));
    S.check "first instance reaped after a second start (no orphans)" (not (S.alive d1));
    (* wait for d2's OWN server to be up AND serving (covers the reap window where :4001 drops) *)
    S.wait_until ~timeout:30.0 (fun () -> body_has page page_path disk);

    (* 2) startup freshness — served content matches the on-disk source (clean checkout) *)
    S.check "test precondition: source is at a clean checkout" (contains (S.read sb src) disk);
    S.check "server serves the current on-disk source on startup" (body_has page page_path disk);

    (* 3) dev cache headers must be no-cache *)
    S.check "page is served no-cache in dev" (cache_no_cache page page_path);
    S.check "client bundle is served no-cache in dev" (cache_no_cache page bundle_path);

    (* 4) edit propagation — SSR and the client bundle must both pick up an edit. The edit lives
       INSIDE with_edit so it is reverted even on failure. *)
    S.with_edit sb src (fun s -> replace s ~old:disk ~by:mark) (fun () ->
        S.wait_until ~timeout:30.0 (fun () -> body_has page bundle_path mark);
        S.check "client bundle picked up the edit" (body_has page bundle_path mark);
        S.wait_until ~timeout:30.0 (fun () -> body_has page page_path mark);
        S.check "SSR picked up the edit" (body_has page page_path mark));

    (* 5) revert propagation — with_edit restored the file; reverting while dev is alive lets
       dune --watch re-sync, leaving no stale _build. Asserting it propagates guards that bug. *)
    S.wait_until ~timeout:30.0 (fun () -> body_has page page_path disk);
    S.check "revert propagated — no stale build left behind" (body_has page page_path disk))
