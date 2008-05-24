module Server =
struct
  let add1 i = i + 1

  let add1_list l = List.map (fun i -> i + 1) l

  let add1_pair (a, b) = (a + 1, b + 1)

  let add1_r { Protocol.fst = f; snd = s; trd = t } =
    {
      Protocol.fst = f + 1;
      snd = (match s with None -> None | Some s -> Some (s + 1));
      trd = Array.map (fun e -> e + 1) t;
    }

  let add1_lst l =
    let rec lst_map f l =
      match l with
        | Protocol.Nil -> Protocol.Nil
        | Protocol.Cons (a,l) -> Protocol.Cons (f a, lst_map f l)
    in
    lst_map (fun i -> i + 1) l

  let addN ?(n = 1) i = i + n

  let maybe_raise flag =
    if flag
    then raise (Protocol.Bar 17)
end
