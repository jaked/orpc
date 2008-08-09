let rec lst_of_list l =
  match l with
    | [] -> Protocol.Nil
    | a::l -> Protocol.Cons (a, lst_of_list l)

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
    (* each result returns before the next request is issued *)
    ignore (C.add1 6);
    ignore (C.addN ~n:7 6);
    ignore (C.add1_list [5;6;7]);
    ignore (C.add1_lst (lst_of_list [7;8;9]));
    ignore (C.add1_pair (17, 22));
    ignore (try C.maybe_raise true with _ -> ())
end

module T =
struct
  let with_formatter f = f Format.err_formatter
end

;;

let module M = Go(Protocol_trace.Sync(T)(Server_impl.Server)) in M.go ();
let module M = Go(Protocol_trace.Sync(T)(Client)) in M.go ();
