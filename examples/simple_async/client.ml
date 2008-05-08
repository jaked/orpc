let rec list_of_lst l =
  match l with
    | Simple_aux.Nil -> []
    | Simple_aux.Cons (a, l) -> a :: list_of_lst l

let rec lst_of_list l =
  match l with
    | [] -> Simple_aux.Nil
    | a::l -> Simple_aux.Cons (a, lst_of_list l)

let print_int i = print_endline (string_of_int i)
let print_int_list l = print_endline (String.concat ", " (List.map string_of_int l))

;;

let esys = Unixqueue.create_unix_event_system() in
let c = Simple_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

Simple_clnt.add1'async c 6 (fun r -> print_int (r ()));
Simple_clnt.add1_list'async c [5;6;7] (fun r -> print_int_list (r ()));
Simple_clnt.add1_lst'async c (lst_of_list [7;8;9]) (fun r -> print_int_list (list_of_lst (r ())));

Unixqueue.run esys;
Rpc_client.shut_down c
