exception Foo
exception Bar of int

module type Abstract =
sig
  type 'a _r

  val add1 : int -> int _r
  val add1_list : int list -> int list _r
  val add1_pair : (int * int) -> (int * int) _r
  val add1_r : Types.r -> Types.r _r
  val add1_lst : int Types.lst -> int Types.lst _r
  val addN : ?n:int -> int -> int _r
  val maybe_raise : bool -> unit _r
end

module type Sync = Abstract with type 'a _r = 'a
module type Async = Abstract with type 'a _r = ((unit -> 'a) -> unit) -> unit
module type Lwt = Abstract with type 'a _r = 'a Lwt.t

(*
module type Sync =
sig
  val add1 : int -> int
  val add1_list : int list -> int list
  val add1_pair : (int * int) -> (int * int)
  val add1_r : Types.r -> Types.r
  val add1_lst : int Types.lst -> int Types.lst
  val addN : ?n:int -> int -> int
  val maybe_raise : bool -> unit
end

module type Async =
sig
  val add1 : int -> ((unit -> int) -> unit) -> unit
  val add1_list : int list -> ((unit -> int list) -> unit) -> unit
  val add1_pair : (int * int) -> ((unit -> (int * int)) -> unit) -> unit
  val add1_r : Types.r -> ((unit -> Types.r) -> unit) -> unit
  val add1_lst : int Types.lst -> ((unit -> int Types.lst) -> unit) -> unit
  val addN : ?n:int -> int -> ((unit -> int) -> unit) -> unit
  val maybe_raise : bool -> ((unit -> unit) -> unit) -> unit
end

module type Lwt =
sig
  val add1 : int -> int Lwt.t
  val add1_list : int list -> int list Lwt.t
  val add1_pair : (int * int) -> (int * int) Lwt.t
  val add1_r : Types.r -> Types.r Lwt.t
  val add1_lst : int Types.lst -> int Types.lst Lwt.t
  val addN : ?n:int -> int -> int Lwt.t
  val maybe_raise : bool -> unit Lwt.t
end
*)
