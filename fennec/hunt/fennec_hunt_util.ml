(* Substring search — allocation-free (no per-position String.sub). Naive O(n·m), which is
   the right trade-off for the small strings test assertions deal with: no setup cost, no
   allocation, predictable. *)
let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  if ln = 0 then true
  else if ln > lh then false
  else begin
    let rec matches_at i j = j = ln || (hay.[i + j] = needle.[j] && matches_at i (j + 1)) in
    let rec scan i = i + ln <= lh && (matches_at i 0 || scan (i + 1)) in
    scan 0
  end
