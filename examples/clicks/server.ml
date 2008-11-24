let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let config_cgi = {
    Netcgi_env.default_config with
      Netcgi_env.permitted_input_content_types = "text/plain" ::
      Netcgi_env.default_config.Netcgi_env.permitted_input_content_types
  } in

  let module M = Proto_js_srv.Sync(struct
    let n = ref 0

    let clicks () = !n
    let click () = incr n; !n
  end) in

  let factories =
    [ Nethttpd_plex.nethttpd_factory
        ~config_cgi
        ~handlers:["clicks", Orpc_js_server.service M.handler]
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
