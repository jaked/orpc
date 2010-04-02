let rec lst_map f l =
  match l with
    | `Nil -> `Nil
    | `Cons (a,l) -> `Cons (f a, lst_map f l)

module Sync =
struct
  type 'a _r = 'a

  let add1 i = i + 1

  let add1_list l = List.map (fun i -> i + 1) l

  let add1_pair (a, b) = (a + 1, b + 1)

  let add1_r { Types.fst = f; snd = s; trd = t } =
    {
      Types.fst = f + 1;
      snd = (match s with None -> None | Some s -> Some (s + 1));
      trd = Array.map (fun e -> e + 1) t;
    }

  let add1_lst l = lst_map (fun i -> i + 1) l

  let addN ?(n = 1) i = i + n

  let maybe_raise flag =
    if flag
    then raise (Protocol.Bar 17)
end

module Async =
struct
  type 'a _r = ((unit -> 'a) -> unit) -> unit

  let add1 i r = r (fun () -> (i + 1))

  let add1_list l r = r (fun () -> (List.map (fun i -> i + 1) l))

  let add1_pair (a, b) r = r (fun () -> (a + 1, b + 1))

  let add1_r { Types.fst = f; snd = s; trd = t } r =
    r (fun () -> {
      Types.fst = f + 1;
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

module Lwt =
struct
  type 'a _r = 'a Lwt.t

  open Lwt

  let add1 i = return (i + 1)

  let add1_list l = return (List.map (fun i -> i + 1) l)

  let add1_pair (a, b) = return (a + 1, b + 1)

  let add1_r { Types.fst = f; snd = s; trd = t } =
    return {
      Types.fst = f + 1;
      snd = (match s with None -> None | Some s -> Some (s + 1));
      trd = Array.map (fun e -> e + 1) t;
    }

  let add1_lst l = return (lst_map (fun i -> i + 1) l)

  let addN ?(n=1) i = return (i + n)

  let maybe_raise flag =
    if flag
    then fail (Protocol.Bar 17)
    else return ()
end
