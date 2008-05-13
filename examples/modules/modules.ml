type 'a lst = Nil | Cons of 'a * 'a lst

module type Sync =
sig
  val add1 : int -> int
  val add1_list : int list -> int list
  val add1_lst : int lst -> int lst
end
