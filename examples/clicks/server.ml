let service handler =
  let process (cgi : Netcgi.cgi_activation) =
    let res =
      try handler (cgi#argument "BODY")#value
      with Not_found -> raise (Invalid_argument "bad_request") in
    (* XXX handle gzip *)
    cgi#set_header
      ~content_type:"text/plain; charset=utf-8"
      ~cache:`No_cache
      ();
    cgi#output#output_string res;
    cgi#output#commit_work () in

  {
    Nethttpd_services.dyn_handler = (fun _ -> process);
    dyn_activation = Nethttpd_services.std_activation `Std_activation_unbuffered;
    dyn_uri = None;
    dyn_translator = (fun _ -> "");
    dyn_accept_all_conditionals = false;
  }

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let config_cgi = {
    Netcgi.default_config with
      Netcgi.permitted_input_content_types = "text/plain" ::
      Netcgi.default_config.Netcgi.permitted_input_content_types
  } in

  let module M = Proto_js_srv.Sync(struct
    let n = ref 0

    let clicks () = !n
    let click () = incr n; !n
  end) in

  let factories =
    [ Nethttpd_plex.nethttpd_factory
        ~config_cgi
        ~handlers:["clicks", service M.handler]
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
