(* PROOF: every output the pre-pass emits is SYNTACTICALLY-VALID mlx.

   The unit table (test_prepass.ml) pins the transform output as an exact string; this complements it
   by feeding a battery of bare-text inputs through the transform AND then through the real `mlx-pp`
   binary, asserting each parses. That closes the loop end to end: bare-text source → pre-pass → mlx.

   `mlx-pp` is found on PATH (installed by the mlx opam package). If it is not installed we SKIP
   (exit 0) rather than fail — the optional mlx dev dependency must not gate unrelated builds. *)

let mlx_pp_on_path () =
  (* `command -v mlx-pp` via the shell; 0 = found *)
  Sys.command "command -v mlx-pp >/dev/null 2>&1" = 0

(* transform [src], write to a temp .mlx, run `mlx-pp` on it; return Ok () if it exits 0 else the
   captured stderr. *)
let parses src =
  let pre = Fennec_mlx_prepass.transform src in
  let tmp = Filename.temp_file "fennec_prepass_" ".mlx" in
  let oc = open_out_bin tmp in
  output_string oc pre; close_out oc;
  let err = Filename.temp_file "fennec_prepass_" ".err" in
  let cmd = Printf.sprintf "mlx-pp %s >/dev/null 2>%s"
              (Filename.quote tmp) (Filename.quote err) in
  let code = Sys.command cmd in
  let msg = let ic = open_in err in
    let n = in_channel_length ic in let s = really_input_string ic n in close_in ic; s in
  (try Sys.remove tmp with _ -> ());
  (try Sys.remove err with _ -> ());
  if code = 0 then Ok () else Error msg

(* a broad battery — every shape the migration relies on, written in the NEW bare-text syntax. *)
let inputs = [
  "let v = <h1>Welcome to Fennec 🦊!</h1>";
  "let v = <a>Sign in</a>";
  "let v = <p>use fun and match and in as plain words</p>";
  "let v = <span>{!count}</span>";
  "let v = <span>{label ^ \":\"}</span>";
  "let v = <p>Hello {name}!</p>";
  "let v = <div>todos in store: {n}</div>";
  "let v = <div><Counter start=3 /><Greeting /></div>";
  "let v = <div><p>one</p><span>two</span></div>";
  "let v = <input value={draft} disabled={not ok} />";
  "let v = <a href=\"/x\" onClick=(go ())>Learn more</a>";
  "let v = <li key=t.id>{t.text}<button onClick=(f g)>remove</button></li>";
  "let v = <li>{f { a = 1; b = 2 }}</li>";
  "[%%style {scss| .hero { color: red; padding: 1rem } |scss}]\nlet v = <section className=\"hero\">Title</section>";
  "let v =\n  <p>\n    multi line\n    indented text\n  </p>";
  "let make ~title ~subtitle () =\n  <section className=\"hero\">\n    <h1 className=\"t\">{title}</h1>\n    <p className=\"s\">{subtitle}</p>\n    <a href=\"/about\">Learn more</a>\n  </section>";
  (* literal parens in bare child text — the closed surprise; must be valid mlx (a quoted string) *)
  "let v = <p>Call us (now) — it's free!</p>";
  "let v = <p>f(x) = y and a (nested (deep)) b</p>";
  "let v = <h2>Say hello (typed client form)</h2>";
  "let v = <p>visits (localStorage): {!n}</p>";
  (* control flow as the {…} escape (JSX-identical), with nested JSX + bare text inside *)
  "let v = <ul>{each xs (fun x -> <li>row {x.id} (#{x.id})</li>)}</ul>";
  "let v = <div>{if ok then <p>yes (really)</p> else <p>no</p>}</div>";
  "let v = <span>{match x with None -> <b>none</b> | Some u -> <i>{name u}</i>}</span>";
]

let () =
  if not (mlx_pp_on_path ()) then begin
    print_endline "parses_through_mlx: mlx-pp not on PATH — SKIPPED"; exit 0
  end;
  let fails = ref 0 in
  List.iter (fun src ->
    match parses src with
    | Ok () -> ()
    | Error msg ->
      incr fails;
      Printf.printf "FAIL parses_through_mlx\n  src: %S\n  pre: %S\n  err: %s\n"
        src (Fennec_mlx_prepass.transform src) msg)
    inputs;
  Printf.printf "parses_through_mlx: %d inputs, %d failed\n" (List.length inputs) !fails;
  if !fails > 0 then exit 1
