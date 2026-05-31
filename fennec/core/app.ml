(* The application: a Plug-style middleware pipeline, explicit server routes, a
   universal page router (anuragsoni/routes), and a customizable 404 — composed
   with the precedence the framework guarantees:

     middleware (may halt) -> explicit server routes -> universal pages -> 404

   [dispatch] is pure (request -> response): it is the whole framework, testable
   without any socket. The Eio server (Fennec_server) is a thin adapter on top. *)

type handler = Http.request -> Http.response

type t = {
  mutable middlewares : Middleware.t list;
  mutable server_routes : (Http.meth * string * handler) list;
  mutable pages : handler Routes.router option;
  mutable not_found : handler;
}

let create () =
  {
    middlewares = [];
    server_routes = [];
    pages = None;
    not_found = (fun _ -> Http.text ~status:404 "404 Not Found");
  }

let use mw t =
  t.middlewares <- t.middlewares @ [ mw ];
  t

let route meth path h t =
  t.server_routes <- t.server_routes @ [ (meth, path, h) ];
  t

let get path h t = route Http.GET path h t
let post path h t = route Http.POST path h t

let pages routes t =
  t.pages <- Some (Routes.one_of routes);
  t

let not_found h t =
  t.not_found <- h;
  t

(* define an isomorphic page route with the routes combinators:
   App.page Routes.(s "tasks" / str /? nil) (fun id req -> ...) *)
let page = Routes.( @--> )

let dispatch t (req : Http.request) : Http.response =
  let conn = Middleware.{ req; resp = None } in
  List.iter (fun mw -> if not (Middleware.halted conn) then mw conn) t.middlewares;
  match conn.resp with
  | Some r -> r (* a middleware short-circuited *)
  | None -> (
    match List.find_opt (fun (m, p, _) -> m = req.meth && p = req.path) t.server_routes with
    | Some (_, _, h) -> h req (* explicit server route wins *)
    | None -> (
      match t.pages with
      | Some router -> (
        match Routes.match' router ~target:req.path with
        | Routes.FullMatch f | Routes.MatchWithTrailingSlash f -> f req
        | Routes.NoMatch -> t.not_found req)
      | None -> t.not_found req))
