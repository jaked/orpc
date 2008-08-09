let rec lst_of_list l =
  match l with
    | [] -> Protocol.Nil
    | a::l -> Protocol.Cons (a, lst_of_list l)

module Go (C : Protocol.Async) =
struct
  let go () =
    (* all requests issued first, then all requests returned (when you call through Client) *)
    C.add1 6 ignore;
    C.addN ~n:7 6 ignore;
    C.add1_list [5;6;7] ignore;
    C.add1_lst (lst_of_list [7;8;9]) ignore;
    C.add1_pair (17, 22) ignore;
    C.maybe_raise true ignore;
end

module T =
struct
  let with_formatter f = f Format.err_formatter
end

;;

let esys = Unixqueue.create_unix_event_system() in
let c = Protocol_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in

let module Client = Protocol_clnt.Async(struct
  let with_client f = f c
end) in

let module M = Go(Protocol_trace.Async(T)(Server_impl.Server)) in M.go ();
let module M = Go(Protocol_trace.Async(T)(Client)) in M.go ();

Unixqueue.run esys;
Rpc_client.shut_down c
