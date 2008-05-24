val add1 : int -> int

(* built-in type constructors *)
val add1_list : int list -> int list

(* tuples *)
val add1_pair : (int * int) -> (int * int)

(* records *)
type r = { fst: int; snd: int option; trd: int array; }

val add1_r : r -> r

(* you can define polymorphic datatypes... *)
type 'a lst = Nil | Cons of 'a * 'a lst

(* but you must pass them at a specific type *)
val add1_lst : int lst -> int lst

(* labelled and optional arguments *)
val addN : ?n:int -> int -> int

(* exceptions *)
exception Foo
exception Bar of int

val maybe_raise : bool -> unit
