(* Property-based testing surface — a thin, lean layer over QCheck2 (see prop.mli).

   The ppx ([let%prop]) funnels EVERY QCheck reference through this module (it emits
   [Fennec_hunt.Prop.Gen.*] / [Fennec_hunt.Prop.check] / …), so a downstream library that
   writes [let%prop] needs only [fennec-hunt] on its [(libraries)] — qcheck-core stays a
   transitive, test-only detail of this package and never reaches the production server or the
   `fennec` binary. *)

module Gen = QCheck2.Gen
module Print = QCheck2.Print

let forall ?count ?print gen prop = QCheck2.Test.make ?count ?print gen prop

let assume = QCheck2.assume

(* type-driven [let%prop]: the ppx derives the generator + printer and bakes the name in here. *)
let check ~name ?print gen prop =
  QCheck2.Test.check_exn (QCheck2.Test.make ~name ?print gen prop)

(* explicit [let%prop = forall …]: the test is already built (anonymous); stamp the let%prop
   name onto it so QCheck's own failure text matches our runner's, then run it. *)
let check_named ~name (QCheck2.Test.Test cell as t) =
  QCheck2.Test.set_name cell name;
  QCheck2.Test.check_exn t

(* curried→tupled adapters: the ppx passes the user's multi-arg lambda verbatim and the
   matching generator produces a tuple, so we uncurry here rather than rebuild the lambda. *)
let uncurry2 f (a, b) = f a b
let uncurry3 f (a, b, c) = f a b c
let uncurry4 f (a, b, c, d) = f a b c d
