(* The /greet PAGE wired into the web server. The conn block (stage 1) computes the cross-stage
   payload from the request, and Page.serve seeds it + SSRs the isomorphic view + emits the page's own
   bundle <script> so it hydrates (stage 2). The ONLY thing that crosses to the client is
   [Greet.codec]'s encoding of the payload — leak-proof by construction (the Conn never crosses). *)
open Fennec.Fur

let page : Greet.t Page.t =
  { key = "page:greet";
    codec = Greet.codec;
    bundle = "/_pages/greet/main.js";
    view = Greet_page.page;
    data =
      (fun conn ->
        let who = match Fennec.Conn.query conn "name" with Some "" | None -> "world" | Some n -> n in
        Page.Render { Greet.who; count = String.length who }) }

let serve = Page.serve page
