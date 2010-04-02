type 'a lst = Nil | Cons of 'a * 'a lst
  with orpc(aux, trace)

type r = { fst: int; snd: int option; trd: int array; }
  with orpc(aux, trace)
