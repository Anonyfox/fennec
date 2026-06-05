(* The DSL + runner instantiated against a real browser (the CDP backend). This is the
   module you open to write tests: [Live.test "…" @@ fun page -> page |> goto … |> click …].
   The identical surface, instantiated with the in-memory fake, is what the unit tests
   drive — so behaviour proven there is the behaviour you get here. *)
include Driver.Make (Cdp_backend)
