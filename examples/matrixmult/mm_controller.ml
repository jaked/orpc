(* Implementation of the controller/multiplier *)

(* The state is kept in global variables for simplicity. This means we can
   only do one multiplication at a time. 
 *)
let current_matrices = ref None
let current_job_queue = ref None
let current_result = ref None


module Controller =
struct
  open Mm_proto_controller

  let ping () = ()

  let with_matrix f =
    match !current_matrices with
      | None ->
	  failwith "No matrix"
      | Some(l,r) ->
	  f l r

  let get_dim which =
    with_matrix
      (fun l r ->
        let m =
	  if which = Left then l else r in
        let rows = Array.length m in
        let cols = if rows > 0 then Array.length m.(0) else 0 in
        { rows = rows;
	  columns = cols
        }
      )

  let get_row which row =
    with_matrix
      (fun l r ->
        let m =
	  if which = Left then l else r in
        m.(row)
      )

  let incr_queue (lc, lc_max, rr, rr_max) =
    let lc' = lc+1 in
    if lc' = lc_max then (
      let rr' = rr+1 in
      if rr' = rr_max then
        None
      else
        Some(0, lc_max, rr', rr_max)
    )
    else
      Some (lc', lc_max, rr, rr_max)


  let rec pull n queue_opt =
    match queue_opt with
      | None ->
	  []
      | Some(lc, lc_max, rr, rr_max) ->
	  (* The "queue" is actually represented as two counters lc and rr *)
	  if n > 0 then (
	    let j = { left_col = lc; right_row = rr } in
	    let queue' = incr_queue (lc, lc_max, rr, rr_max) in
	    current_job_queue := queue';
	    j :: pull (n-1) queue'
	  )
	  else
	    []


  let pull_jobs n =
    let jobs = pull n !current_job_queue in
    (Array.of_list jobs)
	  

  let put_results results =
    match !current_result with
      | None -> 
	  ()
      | Some m ->
	  Array.iter
	    (fun r ->
	      m.( r.res_job.right_row ).( r.res_job.left_col ) <- r.res_val
	    )
	    results;
	  ()
end

module Multiplier (W : sig val workers : (string * int) list end) =
struct

  let ping () = Lwt.return ()

  let fill m rows cols =
    for j = 0 to rows-1 do
      for k = 0 to cols-1 do
        m.(j).(k) <- Random.float 1.0
      done
    done
    

  let test_multiply lrows rcols rrows =
    (* This is an asynchronous RPC implmentation. This means we don't have to
       reply the result immediately. Instead we get an [emit] function, and
       we can call this function at some time in the future to pass the result
       value back to the caller of the RPC. 
    *)
    let lcols = rrows in
    let lmatrix = Array.make_matrix lrows lcols 0.0 in
    let rmatrix = Array.make_matrix rrows rcols 0.0 in
    fill lmatrix lrows lcols;
    fill rmatrix rrows rcols;
    current_matrices := Some(lmatrix,rmatrix);
    current_result := Some(Array.make_matrix rrows lcols 0.0);
    current_job_queue := Some(0, lcols, 0, rrows);
  
    (* Now start the computations by telling all workers to go: *)
    let esys = (Netplex_cenv.self_cont()) # event_system in
    let worker_clients = ref [] in
    let worker_calls =
      List.map
        (fun (host,port) ->
          let worker =
	    Mm_proto_worker_clnt.create_client2
              ~program_number:(Rtypes.uint4_of_int 3)
	      ~esys
	      (`Socket(Rpc.Tcp,
		      Rpc_client.Inet(host,port),
		      Rpc_client.default_socket_config)) in
          worker_clients := worker :: !worker_clients;
          let module M = Mm_proto_worker_clnt.Lwt (struct let with_client f = f worker end) in
          try_lwt M.run ()
          with error ->
  	    Netplex_cenv.logf `Err "Error from worker: %s"
  	      (Printexc.to_string error);
            Lwt.return ())
        W.workers in
    lwt () = Lwt.join worker_calls in
    (* All workers done! *)
    assert(!current_job_queue = None);
    (* Delete the result: *)
    current_matrices := None;
    current_result := None;
    (* XXX original emits reply before shutting down clients---how to achieve in Lwt? *)
    List.iter Rpc_client.shut_down !worker_clients;
    Lwt.return ()
end



let configure cf addr =
  let workers_sect =
    cf # resolve_section addr "worker" in
  let workers =
    List.map
      (fun w_addr ->
	 let host =
	   try cf # string_param(cf # resolve_parameter w_addr "host")
	   with Not_found ->
	     failwith "Required param host is missing" in
	 let port =
	   try cf # int_param(cf # resolve_parameter w_addr "port")
	   with Not_found ->
	     failwith "Required param port is missing" in
	 (host,port)
      )
      workers_sect
  in
  workers
  

let setup srv workers =
  let module W = struct let workers = workers end in
  let module M = Mm_proto_multiplier_srv.Lwt(Multiplier(W)) in
  M.bind
    ~program_number:(Rtypes.uint4_of_int 1)
    srv;
  let module M = Mm_proto_controller_srv.Sync(Controller) in
  M.bind
    ~program_number:(Rtypes.uint4_of_int 2)
    srv


let controller_factory() =
  Rpc_netplex.rpc_factory
    ~configure
    ~name:"mm_controller"
    ~setup
    ~hooks:(fun _ ->
	      ( object(self)
		  inherit Netplex_kit.empty_processor_hooks()
		  method post_start_hook _ =
		    Random.self_init()
		end
	      )
	   )
    ()
