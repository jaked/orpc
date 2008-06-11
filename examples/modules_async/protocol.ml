type 'a lst = Nil | Cons of 'a * 'a lst
type r = { fst: int; snd: int option; trd: int array; }

exception Foo
exception Bar of int

module type Async =
sig
  val add1 : int -> ((unit -> int) -> unit) -> unit
  val add1_list : int list -> ((unit -> int list) -> unit) -> unit
  val add1_pair : (int * int) -> ((unit -> (int * int)) -> unit) -> unit
  val add1_r : r -> ((unit -> r) -> unit) -> unit
  val add1_lst : int lst -> ((unit -> int lst) -> unit) -> unit
  val addN : ?n:int -> int -> ((unit -> int) -> unit) -> unit
  val maybe_raise : bool -> ((unit -> unit) -> unit) -> unit
end
