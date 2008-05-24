let rec list_of_lst l =
  match l with
    | Protocol_aux.Nil -> []
    | Protocol_aux.Cons (a, l) -> a :: list_of_lst l

let rec lst_of_list l =
  match l with
    | [] -> Protocol_aux.Nil
    | a::l -> Protocol_aux.Cons (a, lst_of_list l)

let print_int i = print_endline (string_of_int i)
let print_int_list l = print_endline (String.concat ", " (List.map string_of_int l))
let print_int_lst l = print_int_list (list_of_lst l)
let print_int_pair (a, b) = print_endline ("( " ^ string_of_int a ^ ", " ^ string_of_int b ^ " )")

;;

let esys = Unixqueue.create_unix_event_system() in
let c = Protocol_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

Protocol_clnt.add1'async c 6 (fun r -> print_int (r ()));
Protocol_clnt.addN'async c ~n:7 6 (fun r -> print_int (r ()));
Protocol_clnt.add1_list'async c [5;6;7] (fun r -> print_int_list (r ()));
Protocol_clnt.add1_lst'async c (lst_of_list [7;8;9]) (fun r -> print_int_lst (r ()));
Protocol_clnt.add1_pair'async c (17, 22) (fun r -> print_int_pair (r ()));

Protocol_clnt.maybe_raise'async c true (fun r ->
  try r ()
  with
    | Protocol_aux.Foo -> print_endline "Foo"
    | Protocol_aux.Bar x -> print_endline ("Bar " ^ string_of_int x));

Unixqueue.run esys;
Rpc_client.shut_down c
