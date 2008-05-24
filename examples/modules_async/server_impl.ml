module Server =
struct
  let rec lst_map f l =
    match l with
      | Protocol.Nil -> Protocol.Nil
      | Protocol.Cons (a,l) -> Protocol.Cons (f a, lst_map f l)

  let add1 i r = r (fun () -> (i + 1))

  let add1_list l r = r (fun () -> (List.map (fun i -> i + 1) l))

  let add1_pair (a, b) r = r (fun () -> (a + 1, b + 1))

  let add1_r { Protocol.fst = f; snd = s; trd = t } r =
    r (fun () -> {
      Protocol.fst = f + 1;
      snd = (match s with None -> None | Some s -> Some (s + 1));
      trd = Array.map (fun e -> e + 1) t;
    })

  let add1_lst l r = r (fun () -> (lst_map (fun i -> i + 1) l))

  let addN ?(n=1) i r = r (fun () -> (i + n))

  let maybe_raise flag r =
    r (fun () ->
      if flag
      then raise (Protocol.Bar 17)
      else ())
end
