module type Sync =
sig
  val ping : unit -> unit

  val test_multiply : int -> int -> int -> unit
    (* Creates a test matrix with random values and multiplies them.
       Args are: (l_rows, r_cols, l_cols = r_rows)
    *)

end
