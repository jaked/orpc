type row = float array

type dim = {
  rows : int;
  columns : int;
}

type job = {
  left_col : int;
  right_row : int;
  (* Multiply which column of the left matrix with which row of the right
     matrix
  *)
}

type jobs = job array

type result = {
  res_job : job;
  res_val : float;
}

type results = result array

type which = Left | Right

module type Sync =
sig
  val ping : unit -> unit

  val get_dim : which -> dim
    (* Workers can call this proc to get the dimension of the matrix *)

  val get_row : which -> int -> row
    (* Workers can call this proc to get a row of the matrix. The int
       is the row number, 0..rows-1
    *)

  val pull_jobs : int -> jobs
    (* The controller maintains a queue of jobs. This proc pulls a list
       of jobs from this queue. The int is the requested number.
    *)

  val put_results : results -> unit
    (* Put results into the result matrix. *)

end
