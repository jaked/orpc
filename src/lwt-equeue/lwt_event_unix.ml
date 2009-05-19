let (>>=) = Lwt.bind

let sleep d =
  let chan = Lwt_event.new_channel () in
  let _ =
    Lwt_unix.sleep d >>= fun () ->
    Lwt_event.sync (Lwt_event.send chan ()) in
  Lwt_event.receive chan

let sockop watchers ch name f =
  Lwt_event.behavior (fun performed condition evnum ->
    let res = ref `None in
    let fd = Lwt_unix.unix_file_descr ch in

    let try_op () =
      let e =
        try Lwt_unix.check_descriptor ch; None
        with e -> Some e in
      begin res :=
        match e with
          | None -> `Value (f fd) (* may raise EAGAIN / EWOULDBLOCK from f *)
          | Some e -> `Exn e
      end;
      performed := evnum in

    let poll () =
      try try_op (); true
      with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> false in

    let suspend () =
      ignore (Lwt_unix.register_action watchers ch (fun () ->
        if !performed < 0
        then begin
          try_op (); (* EAGAIN / EWOULDBLOCK caught by Lwt_unix.wrap_syscall *)
          Lwt.wakeup condition ()
        end)) in

    let result () =
      match !res with
        | `None -> invalid_arg name
        | `Value v -> v
        | `Exn e -> raise e in

    { Lwt_event.poll = poll; suspend = suspend; result = result })

let read ch buf pos len =
  sockop
    Lwt_unix.inputs
    ch
    "Lwt_event_unix.read"
    (fun fd -> Unix.read fd buf pos len)

let write ch buf pos len =
  sockop
    Lwt_unix.outputs
    ch
    "Lwt_event_unix.write"
    (fun fd -> Unix.write fd buf pos len)

let accept ch =
  sockop
    Lwt_unix.inputs
    ch
    "Lwt_event_unix.accept"
    (fun fd ->
      let (s, addr) = Unix.accept fd in
      (Lwt_unix.of_unix_file_descr s, addr))
