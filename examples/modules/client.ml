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

module Client = Modules_clnt.Sync(struct
  let with_client f =
    let c = Modules_clnt.create_client (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in
    let r = f c in
    Rpc_client.shut_down c;
    r
end)

module Go (C : Modules.Sync) =
struct
  let go () =
    print_int (C.add1 6);
    print_int_list (C.add1_list [5;6;7]);
    print_int_list (list_of_lst (C.add1_lst (lst_of_list [7;8;9])));
end

;;

let module M = Go(Server_impl.Server) in M.go ();
let module M = Go(Client) in M.go ();
