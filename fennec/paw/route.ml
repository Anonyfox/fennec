(* Route paws — method+path matchers that answer when they match, else decline
   (pass the conn through). These are the [.get]/[.post]/… verbs; each is just a
   paw, so they compose in a pipeline like any other. *)

module H = Fennec_core.Http

(* HEAD is matched as GET (the responder strips the body downstream) *)
let meth_matches (want : H.meth) (got : H.meth) =
  got = want || (want = H.GET && got = H.HEAD)

(* an exact method+path route; [h] is a paw run when it matches *)
let on (m : H.meth) (path : string) (h : Paw.t) : Paw.t =
 fun c -> if meth_matches m (Conn.meth c) && Conn.path c = path then h c else c

let get path h = on H.GET path h
let post path h = on H.POST path h
let put path h = on H.PUT path h
let delete path h = on H.DELETE path h
let patch path h = on H.PATCH path h

(* a fallthrough paw from a [request -> response option] (e.g. static files):
   answers when it yields Some, else declines *)
let fallthrough (f : H.request -> H.response option) : Paw.t =
 fun c -> match f (Conn.req c) with Some r -> Conn.respond c r | None -> c
