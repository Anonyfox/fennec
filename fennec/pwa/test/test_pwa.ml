(* The PWA generation layer: manifest JSON, the service worker, the content-addressed version, and
   the head snippet — all pure strings, asserted natively. *)

let contains hay needle =
  let hl = String.length hay and nl = String.length needle in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let cfg =
  Pwa.v ~scope:"/app" ~theme_color:"#123456"
    ~icons:[ Pwa.icon ~sizes:"512x512" "/icon-512.png" ]
    "My \"Quoted\" App"

let%test "manifest: scope normalized, fields present, strings JSON-escaped" =
  let m = Pwa.manifest cfg in
  contains m {|"scope":"/app/"|}
  && contains m {|"start_url":"/app/"|}
  && contains m {|"name":"My \"Quoted\" App"|}
  && contains m {|"theme_color":"#123456"|}
  && contains m {|"sizes":"512x512"|}

let%test "service worker: cache identity + precache + update + offline-nav semantics are all present" =
  let sw = Pwa.service_worker cfg ~version:"v123" ~precache:[ "/app/main.js"; "/app/main.css" ] in
  contains sw "v123" && contains sw {|"/app/main.js"|} && contains sw {|"/app/main.css"|}
  && contains sw "SKIP_WAITING" && contains sw "skipWaiting" && contains sw "navigate"
  && contains sw {|"/app/"|} (* the offline navigation fallback: the start page *)
  && contains sw "caches.delete" (* atomic swap: old versions die on activate *)

let%test "version_of: content-addressed — same assets same version, changed content changes it" =
  let assets1 = function "/a.js" -> Some "AAA" | "/b.css" -> Some "BBB" | _ -> None in
  let assets2 = function "/a.js" -> Some "AAA-CHANGED" | "/b.css" -> Some "BBB" | _ -> None in
  let v1 = Pwa.version_of ~assets:assets1 [ "/a.js"; "/b.css" ] in
  let v1' = Pwa.version_of ~assets:assets1 [ "/a.js"; "/b.css" ] in
  let v2 = Pwa.version_of ~assets:assets2 [ "/a.js"; "/b.css" ] in
  v1 = v1' && v1 <> v2 && String.length v1 = 12

let%test "head snippet: manifest link, theme color, registration with the right scope, update event" =
  let h = Pwa.head_html cfg in
  contains h {|href="/app/manifest.webmanifest"|}
  && contains h {|content="#123456"|}
  && contains h {|register('/app/sw.js', {scope: '/app/'})|}
  && contains h "fennec:sw-update" && contains h "__fennecApplyUpdate"

let () = exit (Fennec_hunt_unit.run ())
