(* compile-time proof that the real backend satisfies the abstract contract; the
   fake backend in the test suite is checked the same way. *)
module _ : Backend.S = Cdp_backend
