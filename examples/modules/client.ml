let rec lst_of_list l =
  match l with
    | [] -> Protocol.Nil
    | a::l -> Protocol.Cons (a, lst_of_list l)
  
module Sync =
struct
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
      ignore (try C.maybe_raise true with _ -> ());
      ignore (C.add1_string_list ["5";"6";"7"]);
  end
  
  module T = Orpc_pp.Trace_of_formatter(struct let formatter = Format.err_formatter end)
  
  let main () =
    let module M = Go(Protocol_trace.Sync(T)(Server_impl.Sync)) in M.go ();
    let module M = Go(Protocol_trace.Sync(T)(Client)) in M.go ();
end

module Lwt =
struct
  module Go (C : Protocol.Lwt) =
  struct
    open Lwt
  
    let go () =
      (* wait for each result to return before the next request is issued *)
      ignore (
        C.add1 6 >>= fun _ ->
        C.addN ~n:7 6 >>= fun _ ->
        C.add1_list [5;6;7] >>= fun _ ->
        C.add1_lst (lst_of_list [7;8;9]) >>= fun _ ->
        C.add1_pair (17, 22) >>= fun _ ->
        catch
          (fun () -> C.maybe_raise true)
          (function _ -> return ()) >>= fun _ ->
        C.add1_string_list ["5";"6";"7"]
      )
  end
  
  module T = Orpc_pp.Trace_of_formatter(struct let formatter = Format.err_formatter end)
  
  let main () =
    let esys = Unixqueue.create_unix_event_system() in
    let c = Protocol_clnt.create_client ~esys (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in
  
    let module Client = Protocol_clnt.Lwt(struct let with_client f = f c end) in
  
    let module M = Go(Protocol_trace.Lwt(T)(Server_impl.Lwt)) in M.go ();
    let module M = Go(Protocol_trace.Lwt(T)(Client)) in M.go ();
  
    Unixqueue.run esys;
    Rpc_client.shut_down c
end

;;

Sync.main ()
(* Lwt.main () *)
