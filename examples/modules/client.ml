let rec list_of_lst l =
  match l with
    | Protocol.Nil -> []
    | Protocol.Cons (a, l) -> a :: list_of_lst l

let rec lst_of_list l =
  match l with
    | [] -> Protocol.Nil
    | a::l -> Protocol.Cons (a, lst_of_list l)

let print_int i = print_endline (string_of_int i)
let print_int_list l = print_endline (String.concat ", " (List.map string_of_int l))
let print_int_lst l = print_int_list (list_of_lst l)
let print_int_pair (a, b) = print_endline ("( " ^ string_of_int a ^ ", " ^ string_of_int b ^ " )")

module Client = Protocol_clnt.Sync(struct
  let with_client f =
    let c = Protocol_clnt.create_client (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in
    let r = f c in
    Rpc_client.shut_down c;
    r
end)

module Go (C : Protocol.Sync) =
struct
  let go () =
    print_int (C.add1 6);
    print_int (C.addN ~n:7 6);
    print_int_list (C.add1_list [5;6;7]);
    print_int_lst (C.add1_lst (lst_of_list [7;8;9]));
    print_int_pair (C.add1_pair (17, 22));

    try C.maybe_raise true
    with
      | Protocol.Foo -> print_endline "Foo"
      | Protocol.Bar x -> print_endline ("Bar " ^ string_of_int x);
end

;;

let module M = Go(Server_impl.Server) in M.go ();
let module M = Go(Client) in M.go ();
