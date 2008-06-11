type 'a lst = Nil | Cons of 'a * 'a lst
type r = { fst: int; snd: int option; trd: int array; }

exception Foo
exception Bar of int

module type Lwt =
sig
  val add1 : int -> int Lwt.t
  val add1_list : int list -> int list Lwt.t
  val add1_pair : (int * int) -> (int * int) Lwt.t
  val add1_r : r -> r Lwt.t
  val add1_lst : int lst -> int lst Lwt.t
  val addN : ?n:int -> int -> int Lwt.t
  val maybe_raise : bool -> unit Lwt.t
end
