module Server =
struct
  let rec lst_map f l =
    match l with
      | Modules.Nil -> Modules.Nil
      | Modules.Cons (a,l) -> Modules.Cons (f a, lst_map f l)

  let add1 i = i + 1
  let add1_list l = List.map add1 l
  let add1_lst l = lst_map add1 l
end
