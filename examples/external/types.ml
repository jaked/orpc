type 'a lst = Nil | Cons of 'a * 'a lst
  with orpc_aux, orpc_trace

type r = { fst: int; snd: int option; trd: int array; }
  with orpc_aux, orpc_trace
