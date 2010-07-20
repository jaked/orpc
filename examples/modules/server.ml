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
        ~setup:(fun srv () ->
                  let module M = Protocol_srv.Sync(Server_impl.Sync) in
                  (* let module M = Protocol_srv.Lwt(Server_impl.Lwt) in *)
                  M.bind srv)
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
