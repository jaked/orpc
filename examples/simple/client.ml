let rec lst_of_list l =
  match l with
    | [] -> Protocol_aux.Nil
    | a::l -> Protocol_aux.Cons (a, lst_of_list l)

module Pp = Protocol_trace.Pp

;;

let c = Protocol_clnt.create_client (Rpc_client.Inet ("localhost", 9007)) Rpc.Tcp in
let fmt = Format.err_formatter in

Pp.pp_add1'reply fmt (Pp.pp_add1'call fmt 6; Protocol_clnt.add1 c 6);
Pp.pp_addN'reply fmt (Pp.pp_addN'call fmt ~n:7 6; Protocol_clnt.addN c ~n:7 6);
Pp.pp_add1_list'reply fmt (Pp.pp_add1_list'call fmt [5;6;7]; Protocol_clnt.add1_list c [5;6;7]);
Pp.pp_add1_lst'reply fmt (Pp.pp_add1_lst'call fmt (lst_of_list [7;8;9]); Protocol_clnt.add1_lst c (lst_of_list [7;8;9]));
Pp.pp_add1_pair'reply fmt (Pp.pp_add1_pair'call fmt (17, 22); Protocol_clnt.add1_pair c (17, 22));

try Pp.pp_maybe_raise'reply fmt (Pp.pp_maybe_raise'call fmt true; Protocol_clnt.maybe_raise c true)
with e -> Pp.pp_exn'reply fmt e;

Rpc_client.shut_down c
