type 'a lst = Nil | Cons of 'a * 'a lst
type r = { fst: int; snd: int option; trd: int array; }

exception Foo
exception Bar of int

module type Abstract =
sig
  type 'a _r

  val add1 : int -> int _r
  val add1_list : int list -> int list _r
  val add1_pair : (int * int) -> (int * int) _r
  val add1_r : r -> r _r
  val add1_lst : int lst -> int lst _r
  val addN : ?n:int -> int -> int _r
  val maybe_raise : bool -> unit _r
end

module type Sync = Abstract with type 'a _r = 'a
module type Lwt = Abstract with type 'a _r = 'a Lwt.t

(*
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
*)
