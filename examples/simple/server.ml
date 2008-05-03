let rec lst_map f l =
  match l with
    | Simple_aux.Nil -> Simple_aux.Nil
    | Simple_aux.Cons (a,l) -> Simple_aux.Cons (f a, lst_map f l)

let proc_add1 i = i + 1
let proc_add1_list l = List.map proc_add1 l
let proc_add1_lst l = lst_map proc_add1 l

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let factories =
    [ Rpc_netplex.rpc_factory
        ~configure:(fun _ _ -> ())
        ~name:"add1"
        ~setup:(fun srv () -> Simple_srv.bind ~proc_add1 ~proc_add1_list ~proc_add1_lst srv)
        ();
    ]
  in

  Netplex_main.startup
    (Netplex_mp.mp())
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    factories
    cmdline_cfg
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start()
;;
