let rec list_of_lst l =
  match l with
    | Modules.Nil -> []
    | Modules.Cons (a, l) -> a :: list_of_lst l

let rec lst_of_list l =
  match l with
    | [] -> Modules.Nil
    | a::l -> Modules.Cons (a, lst_of_list l)

let print_int i = print_endline (string_of_int i)
let print_int_list l = print_endline (String.concat ", " (List.map string_of_int l))

module Go (C : Modules.Async) =
struct
  let go () =
    C.add1 6 print_int;
    C.add1_list [5;6;7] print_int_list;
    C.add1_lst (lst_of_list [7;8;9]) (fun r -> print_int_list (list_of_lst r));
end

;;

let esys = Unixqueue.create_unix_event_system() in
let c = Modules_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

let module Client = Modules_clnt.Async(struct
  let with_client f = f c
end) in

let module M = Go(Server_impl.Server) in M.go ();
let module M = Go(Client) in M.go ();

Unixqueue.run esys;
Rpc_client.shut_down c
