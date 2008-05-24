type 'a lst = Nil | Cons of 'a * 'a lst
type r = { fst: int; snd: int option; trd: int array; }

exception Foo
exception Bar of int

module type Sync =
sig
  val add1 : int -> int
  val add1_list : int list -> int list
  val add1_pair : (int * int) -> (int * int)
  val add1_r : r -> r
  val add1_lst : int lst -> int lst
  val addN : ?n:int -> int -> int
  val maybe_raise : bool -> unit
end
