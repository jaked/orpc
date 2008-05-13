module Server =
struct
  let rec lst_map f l =
    match l with
      | Modules.Nil -> Modules.Nil
      | Modules.Cons (a,l) -> Modules.Cons (f a, lst_map f l)

  let add1 i r = r (i + 1)
  let add1_list l r = r (List.map (fun i -> i + 1) l)
  let add1_lst l r = r (lst_map (fun i -> i + 1) l)
end
