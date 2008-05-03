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

let c = Simple_clnt.create_client (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

print_int (Simple_clnt.add1 c 6);
print_int_list (Simple_clnt.add1_list c [5;6;7]);
print_int_list (list_of_lst (Simple_clnt.add1_lst c (lst_of_list [7;8;9])));

Rpc_client.shut_down c
