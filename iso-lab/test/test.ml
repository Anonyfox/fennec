(* Contract lock for the Fur runtime core. Pure algorithms only — signals, matcher,
   head merge, SSR rendering, data resources, router. Reconcile/hydrate are covered by
   the jsdom E2E until the backend-abstract refactor makes them unit-testable. *)
let passed = ref 0 and failed = ref 0
let check name cond =
  if cond then incr passed else (incr failed; Printf.printf "  \xe2\x9c\x97 %s\n" name)
let eq name a b = check (Printf.sprintf "%s (= %s)" name a) (a = b)
let contains hay needle =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i+1)) in m = 0 || go 0

let () = print_endline "— signals —";
  let s = Iso.signal 0 in
  check "peek initial" (Iso.peek s = 0);
  let runs = ref 0 in
  Iso.run_effect { Iso.run = (fun () -> incr runs; ignore (Iso.get s)); deps = [] };
  check "effect runs once on create" (!runs = 1);
  Iso.set s 1;            check "effect re-runs on change" (!runs = 2);
  Iso.set s 1;            check "no re-run on equal set" (!runs = 2);
  Iso.update s (fun n -> n + 1); check "update notifies" (!runs = 3 && Iso.peek s = 2);
  let r2 = ref 0 in
  let e = { Iso.run = (fun () -> incr r2; ignore (Iso.get s)); deps = [] } in
  Iso.run_effect e; let before = !r2 in
  Iso.dispose e; Iso.set s 99;
  check "disposed effect never re-runs" (!r2 = before);
  let a = Iso.signal 1 and b = Iso.signal 10 and pick = Iso.signal true in
  let last = ref 0 in
  Iso.run_effect { Iso.run = (fun () -> last := if Iso.get pick then Iso.get a else Iso.get b); deps = [] };
  check "dynamic deps: tracks a" (!last = 1);
  Iso.set pick false; check "switches to b" (!last = 10);
  Iso.set a 5;         check "no longer tracks a after switch" (!last = 10)

let () = print_endline "— matcher —";
  let open Iso.Matcher in
  check "root" (match_one ~pattern:"/" "/" = Some []);
  check "exact" (match_one ~pattern:"/about" "/about" = Some []);
  check "named param" (match_one ~pattern:"/users/:id" "/users/42" = Some [("id","42")]);
  check "two params" (match_one ~pattern:"/p/:a/:b" "/p/x/y" = Some [("a","x");("b","y")]);
  check "catch-all" (match_one ~pattern:"/files/*" "/files/a/b" = Some [("*","a/b")]);
  check "no match (len)" (match_one ~pattern:"/users/:id" "/users" = None);
  check "no match (lit)" (match_one ~pattern:"/a" "/b" = None);
  check "trailing slash normalizes" (match_one ~pattern:"/about" "/about/" = Some []);
  let table = [("/", `Home); ("/products", `List); ("/products/:id", `Show)] in
  check "find first-match" (find table "/products" = Some (`List, []));
  check "find param" (find table "/products/7" = Some (`Show, [("id","7")]));
  check "param accessor" (param [("id","7")] "id" = Some "7")

let () = print_endline "— head merge —";
  let open Iso.Head in
  let r = resolve [ (0, [Tag.title "A"; Tag.meta ~name:"description" "old"]);
                    (1, [Tag.title "B"; Tag.meta ~name:"description" "new"; Tag.og "og:x" "y"]) ] in
  check "title last wins" (List.exists (function Title "B" -> true | _ -> false) r);
  check "stale title dropped" (not (List.exists (function Title "A" -> true | _ -> false) r));
  check "meta deduped by name (last)" (List.exists (function Meta a -> List.assoc_opt "content" a = Some "new" | _ -> false) r);
  check "non-conflicting kept" (List.exists (function Meta a -> List.assoc_opt "property" a = Some "og:x" | _ -> false) r);
  eq "tag_key title" (tag_key (Tag.title "z")) "title";
  eq "tag_key meta" (tag_key (Tag.meta ~name:"description" "z")) "meta:description"

let () = print_endline "— SSR (to_html) —";
  eq "escape text" (Iso.to_html (Iso.text "<a&\"b>")) "&lt;a&amp;&quot;b&gt;";
  eq "attr escape" (Iso.to_html (Iso.h "div" [Iso.attr "class" "x" ] [])) "<div class=\"x\"></div>";
  eq "void self-close" (Iso.to_html (Iso.h "input" [] [])) "<input/>";
  eq "handlers omitted in ssr" (Iso.to_html (Iso.h "button" [Iso.on "click" (fun () -> ())] [Iso.text "go"])) "<button>go</button>";
  eq "fragment concats" (Iso.to_html (Iso.frag [Iso.text "a"; Iso.text "b"])) "ab";
  eq "adjacent text coalesces" (Iso.to_html (Iso.h "p" [] [Iso.text "x"; Iso.text "y"])) "<p>xy</p>";
  eq "raw passthrough" (Iso.to_html (Iso.raw "<x>")) "<x>";
  eq "doctype" (String.sub (Iso.document (Iso.h "html" [] [])) 0 9) "<!doctype"

let () = print_endline "— data resources —";
  Iso.Data.clear_seed ();
  Iso.Data.put_seed "k" "v";
  let hit = Iso.Data.string "k" ~fallback:"f" () in
  eq "seeded resource is ready value" (Iso.Data.value hit) "v";
  check "seeded not loading" (not (Iso.Data.loading hit));
  Iso.Data.source := (fun _ _ -> ());  (* miss -> stays loading, fallback shown *)
  let miss = Iso.Data.string "absent" ~fallback:"f" () in
  eq "miss shows fallback" (Iso.Data.value miss) "f";
  check "miss is loading" (Iso.Data.loading miss);
  Iso.Data.clear_seed (); Iso.Data.put_seed "u" "ok";
  let s = Iso.Data.to_script () in
  check "to_script assigns global" (contains s "window.__ISO_DATA__={");
  check "to_script contains pair" (contains s "\"u\":\"ok\"");
  check "to_script escapes <" (let v = Iso.Data.js_string "<x>" in v = "\"\\u003cx>\"")

let () = print_endline "— router —";
  let open Iso.Router in
  eq "relativize strips base" (relativize "/shop" "/shop/products") "/products";
  eq "relativize base->root" (relativize "/shop" "/shop") "/";
  eq "absolutize prefixes" (absolutize "/shop" "/products") "/shop/products";
  eq "absolutize root->base" (absolutize "/shop" "/") "/shop";
  eq "root base passthrough" (relativize "" "/x") "/x";
  let dummy _ = fun () -> Iso.text "" in
  let t = make ~base:"/shop" () |> page ~name:"product" "/products/:id" dummy |> page ~name:"home" "/" dummy in
  eq "reverse build" (build t "product" [("id","7")]) "/products/7";
  eq "href base-prefixed" (href t "product" [("id","7")]) "/shop/products/7";
  eq "typed path" (path t "/products/%d" 7) "/shop/products/7";
  eq "ext raw" (ext "/admin/%d" 3) "/admin/3"

let () =
  Printf.printf "\n%s — %d passed, %d failed\n" (if !failed = 0 then "PASS" else "FAIL") !passed !failed;
  exit (if !failed = 0 then 0 else 1)
