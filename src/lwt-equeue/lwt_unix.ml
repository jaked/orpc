(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Original file (lwt_unix.ml in the Lwt source distribution) is
 * Copyright (C) 2005-2008 Jérôme Vouillon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

let setup_events = ref (fun () -> failwith "setup_events unset")

module SleepQueue =
  Pqueue.Make (struct
    type t = float * unit Lwt.t
    let compare (t, _) (t', _) = compare t t'
  end)
let sleep_queue = ref SleepQueue.empty
let new_sleeps = ref []

let sleep d =
  let res = Lwt.wait () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  new_sleeps := (t, res) :: !new_sleeps;
  !setup_events ();
  res

let yield () = sleep 0.

let get_time t =
  if !t = -1. then t := Unix.gettimeofday ();
  !t

let in_the_past now t =
  t = 0. || t <= get_time now

let rec restart_threads now =
  match
    try Some (SleepQueue.find_min !sleep_queue) with Not_found -> None
  with
    Some (time, thr) when in_the_past now time ->
      sleep_queue := SleepQueue.remove_min !sleep_queue;
      Lwt.wakeup thr ();
      restart_threads now
  | _ ->
      ()

(****)

type state = Open | Closed | Aborted of exn

type file_descr = { fd : Unix.file_descr; mutable state: state }

let mk_ch fd =
  Unix.set_nonblock fd;
  { fd = fd; state = Open }

let check_descriptor ch =
  match ch.state with
    Open ->
      ()
  | Aborted e ->
      raise e
  | Closed ->
      raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

(****)

module FdMap =
  Map.Make (struct type t = Unix.file_descr let compare = compare end)

type watchers = (file_descr * (unit -> unit) list ref) FdMap.t ref

let inputs = ref FdMap.empty
let outputs = ref FdMap.empty

exception Retry
exception Retry_write
exception Retry_read

let find_actions set ch =
  try
    FdMap.find ch.fd !set
  with Not_found ->
    let res = (ch, ref []) in
    set := FdMap.add ch.fd res !set;
    res

type 'a outcome =
    Success of 'a
  | Exn of exn
  | Requeued

let rec wrap_syscall set ch cont action =
  let res =
    try
      check_descriptor ch;
      Success (action ())
    with
      Retry
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io ->
        (* EINTR because we are catching SIG_CHLD hence the system call
           might be interrupted to handle the signal; this lets us restart
           the system call eventually. *)
        add_action set ch cont action;
        Requeued
    | Retry_read ->
        add_action inputs ch cont action;
        Requeued
    | Retry_write ->
        add_action outputs ch cont action;
        Requeued
    | e ->
        Exn e
  in
  match res with
    Success v ->
      Lwt.wakeup cont v
  | Exn e ->
      Lwt.wakeup_exn cont e
  | Requeued ->
      ()

and add_action set ch cont action =
  assert (ch.state = Open);
  let (_, actions) = find_actions set ch in
  actions := (fun () -> wrap_syscall set ch cont action) :: !actions;
  !setup_events ()

let register_action set ch action =
  let cont = Lwt.wait () in
  add_action set ch cont action;
  cont

let perform_actions set fd =
  try
    let (ch, actions) = FdMap.find fd !set in
    set := FdMap.remove fd !set;
    List.iter (fun f -> f ()) !actions
  with Not_found ->
    ()

let active_descriptors set = FdMap.fold (fun key _ l -> key :: l) !set []

let blocked_thread_count set =
  FdMap.fold (fun key (_, l) c -> List.length !l + c) !set 0

(****)

let wait_children = ref []

(*
  if Unixqueue's select is interrupted by EINTR then there are two
  Signal events, but I think this is no harm (we just check for
  completed children an extra time).

  the Unixqueue manual says that signals may be missed (by catching
  only EINTR from select), and that there are race conditions in
  OCaml's signal handling. but handling SIGCHLD is good enough for
  stock Lwt_unix.
*)
let _ =
  ignore (Sys.signal Sys.sigchld
             (Sys.Signal_handle (fun _ ->
               try
                 let es = Lwt_equeue.event_system () in
                 es#add_event Unixqueue.Signal
               with Lwt_equeue.Event_system_not_set -> ())))

(* it doesn't matter what event system we create a group on, it's just a token *)
(* XXX but once it is terminated we cannot reuse it *)
let group = (Unixqueue.create_unix_event_system ())#new_group ()

let old_events = ref []

let has_handler = ref false

let rec setup_events' () =
  sleep_queue :=
    List.fold_left
      (fun q e -> SleepQueue.add e q) !sleep_queue !new_sleeps;
  new_sleeps := [];
  let next_event =
    try
      let (time, _) = SleepQueue.find_min !sleep_queue in Some time
    with Not_found ->
      None
  in
  let now = ref (-1.) in
  let delay =
    match next_event with
        None      -> -1.
      | Some 0.   -> 0.
      | Some time -> max 0. (time -. get_time now)
  in
  let infds = active_descriptors inputs in
  let outfds = active_descriptors outputs in
  let es = Lwt_equeue.event_system () in
  List.iter (fun e -> es#remove_resource group e) !old_events;
  let new_events =
    Unixqueue.Wait (es#new_wait_id ()) ::
      List.map (fun fd -> Unixqueue.Wait_in fd) infds @
      List.map (fun fd -> Unixqueue.Wait_out fd) outfds in
  old_events := new_events;
  ListLabels.iter new_events ~f:(fun e ->
    let timeout = match e with Unixqueue.Wait _ -> delay | _ -> -1. in
    es#add_resource group (e, timeout));
  if not !has_handler
  then begin
    has_handler := true;
    es#add_handler group handler
  end

and handler es _ e =
  let readers, writers, child_exited =
    match e with
      | Unixqueue.Input_arrived (_, fd) -> [ fd ], [], false
      | Unixqueue.Output_readiness (_, fd) -> [], [ fd ], false
      | Unixqueue.Timeout _ -> [], [], false
      | Unixqueue.Extra Lwt_equeue.Shutdown -> es#clear group; raise Equeue.Reject
      | Unixqueue.Signal -> [], [], true
      | _ -> raise Equeue.Reject in

  (*
    we call !setup_events when making blocking calls, because there is
    no other opportunity to set up the Unixqueue events. (maybe Unixqueue
    should have a pre-select hook.)

    however, we need to be careful not to move new_sleeps into
    sleep_queue while restart_threads is running, or else threads may
    not be scheduled fairly. (e.g. let rec t () = yield () >>= t would
    starve other threads)

    since we call setup_events' at the end of the handler it is safe
    not to call it within the handler.
  *)
  setup_events := ignore;
  let now = ref (-1.) in
  restart_threads now;
  List.iter (fun fd -> perform_actions inputs fd) readers;
  List.iter (fun fd -> perform_actions outputs fd) writers;
  if child_exited then begin
    let l = !wait_children in
    wait_children := [];
    List.iter
      (fun ((cont, flags, pid) as e) ->
        try
          let (pid', _) as v = Unix.waitpid flags pid in
          if pid' = 0 then
            wait_children := e :: !wait_children
          else
            Lwt.wakeup cont v
        with e ->
          Lwt.wakeup_exn cont e)
      l
  end;
  setup_events := setup_events';
  setup_events' ();
  if e = Unixqueue.Signal then raise Equeue.Reject

;;
setup_events := setup_events'

let run t =
  (Lwt_equeue.event_system ())#run ();
  match Lwt.poll t with
    | Some v -> v
    | None -> failwith "Lwt_unix: thread did not complete"

(****)

let set_state ch st =
  ch.state <- st;
  perform_actions inputs ch.fd;
  perform_actions outputs ch.fd

let abort ch e =
  if ch.state <> Closed then
    set_state ch (Aborted e)

let unix_file_descr ch = ch.fd

let of_unix_file_descr fd = mk_ch fd

let read ch buf pos len =
  try
    check_descriptor ch;
    Lwt.return (Unix.read ch.fd buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      register_action inputs ch (fun () -> Unix.read ch.fd buf pos len)
  | e ->
      Lwt.fail e

let write ch buf pos len =
  try
    check_descriptor ch;
    Lwt.return (Unix.write ch.fd buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      register_action outputs ch (fun () -> Unix.write ch.fd buf pos len)
  | e ->
      Lwt.fail e

let wait_read ch = register_action inputs ch (fun () -> ())
let wait_write ch = register_action outputs ch (fun () -> ())

let pipe () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch out_fd, mk_ch in_fd)

let pipe_in () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch out_fd, in_fd)

let pipe_out () =
  let (out_fd, in_fd) = Unix.pipe() in
  (out_fd, mk_ch in_fd)

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  mk_ch s

let shutdown ch shutdown_command =
  check_descriptor ch;
  Unix.shutdown ch.fd shutdown_command

let socketpair dom typ proto =
  let (s1, s2) = Unix.socketpair dom typ proto in
  (mk_ch s1, mk_ch s2)

let accept ch =
  try
    check_descriptor ch;
    register_action inputs ch
      (fun () ->
         let (s, addr) = Unix.accept ch.fd in
         (mk_ch s, addr))
  with e ->
    Lwt.fail e

let check_socket ch =
  register_action outputs ch
    (fun () ->
       try ignore (Unix.getpeername ch.fd) with
         Unix.Unix_error (Unix.ENOTCONN, _, _) ->
           (* Get the socket error *)
           ignore (Unix.read ch.fd " " 0 1))

let connect ch addr =
  try
    check_descriptor ch;
    Unix.connect ch.fd addr;
    Lwt.return ()
  with
    Unix.Unix_error
      ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        check_socket ch
  | e ->
      Lwt.fail e

let _waitpid flags pid =
  try
    Lwt.return (Unix.waitpid flags pid)
  with e ->
    Lwt.fail e

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    Lwt.bind (_waitpid flags pid) (fun ((pid', _) as res) ->
    if pid' <> 0 then
      Lwt.return res
    else
      let res = Lwt.wait () in
      wait_children := (res, flags, pid) :: !wait_children;
      res)

let wait () = waitpid [] (-1)

let system cmd =
  match Unix.fork () with
     0 -> begin try
            Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ ->
            exit 127
          end
  | id -> Lwt.bind (waitpid [] id) (fun (pid, status) -> Lwt.return status)

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  Unix.close ch.fd

let setsockopt ch opt v =
  check_descriptor ch;
  Unix.setsockopt ch.fd opt v

let bind ch addr =
  check_descriptor ch;
  Unix.bind ch.fd addr

let listen ch cnt =
  check_descriptor ch;
  Unix.listen ch.fd cnt

let set_close_on_exec ch =
  check_descriptor ch;
  Unix.set_close_on_exec ch.fd

(****)

(*
type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output toclose =
  match Unix.fork () with
     0 -> if input <> Unix.stdin then begin
            Unix.dup2 input Unix.stdin;
            Unix.close input
          end;
          if output <> Unix.stdout then begin
            Unix.dup2 output Unix.stdout;
            Unix.close output
          end;
          List.iter Unix.close toclose;
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe_in () in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) Unix.stdin in_write [in_read];
  Unix.close in_write;
  Lwt.return inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe_out () in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read Unix.stdout [out_write];
  Unix.close out_read;
  Lwt.return outchan

let open_process cmd =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  Unix.close out_read;
  Unix.close in_write;
  Lwt.return (inchan, outchan)

let open_proc_full cmd env proc input output error toclose =
  match Unix.fork () with
     0 -> Unix.dup2 input Unix.stdin; Unix.close input;
          Unix.dup2 output Unix.stdout; Unix.close output;
          Unix.dup2 error Unix.stderr; Unix.close error;
          List.iter Unix.close toclose;
          Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let (err_read, err_write) = pipe_in () in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  open_proc_full cmd env (Process_full(inchan, outchan, errchan))
                 out_read in_write err_write [in_read; out_write; err_read];
  Unix.close out_read;
  Unix.close in_write;
  Unix.close err_write;
  Lwt.return (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise (Unix.Unix_error (Unix.EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan; close_out outchan; close_in errchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)
*)

(****)

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = List.length !wait_children
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
