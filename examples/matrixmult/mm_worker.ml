(* Implementation of the worker *)

module Worker
  (Controller_cfg : sig
    val controller_host : string
    val controller_port : int
  end) =
struct
  open Mm_proto_controller


  let jobs_at_once = 100

  let ping () = ()

  let run () =
    let controller =
      Mm_proto_controller_clnt.create_client2
        ~program_number:(Rtypes.uint4_of_int 2)
        (`Socket(Rpc.Tcp,
	        Rpc_client.Inet(Controller_cfg.controller_host,Controller_cfg.controller_port),
	        Rpc_client.default_socket_config)) in

    let module Controller =
      Mm_proto_controller_clnt.Sync(struct let with_client f = f controller end) in

    (* Get the dimension of the matrices, and retrieve them from the ctrl:*)
    let ldim = Controller.get_dim Left in
    let rdim = Controller.get_dim Right in
    assert(ldim.rows = rdim.columns);

    let lmatrix =
      Array.make ldim.rows [| |] in
    for j = 0 to ldim.rows-1 do
      lmatrix.(j) <- Controller.get_row Left j
    done;
    let rmatrix =
      Array.make rdim.rows [| |] in
    for j = 0 to ldim.rows-1 do
      rmatrix.(j) <- Controller.get_row Right j
    done;
  
    (* Get jobs until there are no more jobs. *)
    let cont = ref true in
    while !cont do
      let jobs = Controller.pull_jobs jobs_at_once in
      cont := (jobs <> [| |]);
    
      let results = ref [] in
      Array.iter
        (fun job ->
	  let lcol = job.left_col in
	  let rrow = job.right_row in
	  let s = ref 0.0 in
	  for j = 0 to ldim.rows-1 do
	    s := !s +. lmatrix.(j).(lcol) *. rmatrix.(rrow).(j)
	  done;
	  results :=
	    { res_job = job;
	      res_val = !s
	    } :: !results
        )
        jobs;
    
      Controller.put_results (Array.of_list !results)
    done;

    (* Done: return "()" to caller *)
    ()
end


let configure cf addr =
  let controller_host =
    try cf # string_param(cf # resolve_parameter addr "controller_host")
    with Not_found ->
      failwith "Required param controller_host is missing" in
  let controller_port =
    try cf # int_param(cf # resolve_parameter addr "controller_port")
    with Not_found ->
      failwith "Required param controller_port is missing" in
  (controller_host, controller_port)
  

let setup srv (controller_host, controller_port) =
  let module Controller_cfg =
      struct
        let controller_host = controller_host
        let controller_port = controller_port
      end in
  let module M = Mm_proto_worker_srv.Sync(Worker(Controller_cfg)) in
  M.bind
    ~program_number:(Rtypes.uint4_of_int 3)
    srv

let worker_factory() =
  Rpc_netplex.rpc_factory
    ~configure
    ~name:"mm_worker"
    ~setup
    ()
