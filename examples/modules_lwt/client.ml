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

module Go (C : Protocol.Lwt) =
struct
  open Lwt

  let go () =
    ignore_result (C.add1 6 >>= fun r -> return (print_int r));
    ignore_result (C.addN ~n:7 6 >>= fun r -> return (print_int r));
    ignore_result (C.add1_list [5;6;7] >>= fun r -> return (print_int_list r));
    ignore_result (C.add1_lst (lst_of_list [7;8;9]) >>= fun r -> return (print_int_list (list_of_lst r)));
    ignore_result (C.add1_pair (17, 22) >>= fun r -> return (print_int_pair r));

    ignore_result (catch
                     (fun () -> C.maybe_raise true)
                     (function
                       | Protocol.Foo -> return (print_endline "Foo")
                       | Protocol.Bar x -> return (print_endline ("Bar " ^ string_of_int x))
                       | e -> return ()));
end

;;

let esys = Unixqueue.create_unix_event_system() in
let c = Protocol_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

let module Client = Protocol_clnt.Lwt(struct
  let with_client f = f c
end) in

let module M = Go(Server_impl.Server) in M.go ();
let module M = Go(Client) in M.go ();

Unixqueue.run esys;
Rpc_client.shut_down c
