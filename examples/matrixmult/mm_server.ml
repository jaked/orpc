(* The main program for the server netplex *)

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let parallelizer = Netplex_mp.mp() in (* multi-processing *)
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ Mm_worker.worker_factory();
      Mm_controller.controller_factory()
    ]
    cmdline_cfg


let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  start()

