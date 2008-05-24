let rec list_of_lst l =
  match l with
    | Protocol_aux.Nil -> []
    | Protocol_aux.Cons (a, l) -> a :: list_of_lst l

let rec lst_of_list l =
  match l with
    | [] -> Protocol_aux.Nil
    | a::l -> Protocol_aux.Cons (a, lst_of_list l)

let print_int i = print_endline (string_of_int i)
let print_int_list l = print_endline ("[ " ^ String.concat "; " (List.map string_of_int l) ^ " ]")
let print_int_lst l = print_int_list (list_of_lst l)
let print_int_pair (a, b) = print_endline ("( " ^ string_of_int a ^ ", " ^ string_of_int b ^ " )")

;;

let c = Protocol_clnt.create_client (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

print_int (Protocol_clnt.add1 c 6);
print_int (Protocol_clnt.addN c ~n:7 6);
print_int_list (Protocol_clnt.add1_list c [5;6;7]);
print_int_lst (Protocol_clnt.add1_lst c (lst_of_list [7;8;9]));
print_int_pair (Protocol_clnt.add1_pair c (17, 22));

try Protocol_clnt.maybe_raise c true
with
  | Protocol_aux.Foo -> print_endline "Foo"
  | Protocol_aux.Bar x -> print_endline ("Bar " ^ string_of_int x);

Rpc_client.shut_down c
