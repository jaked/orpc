let proc_add1 i = i + 1

let proc_add1_list l = List.map (fun i -> i + 1) l

let proc_add1_pair (a, b) = (a + 1, b + 1)

let proc_add1_r { Protocol_aux.fst = f; snd = s; trd = t } =
  {
    Protocol_aux.fst = f + 1;
    snd = (match s with None -> None | Some s -> Some (s + 1));
    trd = Array.map (fun e -> e + 1) t;
  }

let proc_add1_lst l =
  let rec lst_map f l =
    match l with
      | Protocol_aux.Nil -> Protocol_aux.Nil
      | Protocol_aux.Cons (a,l) -> Protocol_aux.Cons (f a, lst_map f l)
  in
  lst_map (fun i -> i + 1) l

let proc_addN ?(n = 1) i = i + n

let proc_maybe_raise flag =
  if flag
  then raise (Protocol_aux.Bar 17)

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
          Protocol_srv.bind
            ~proc_add1 ~proc_add1_list ~proc_add1_pair
            ~proc_add1_r ~proc_add1_lst ~proc_addN
            ~proc_maybe_raise
            srv)
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
