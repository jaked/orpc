let (>>=) = Lwt.bind

let sleep d =
  let chan = Lwt_event.new_channel () in
  let _ =
    Lwt_unix.sleep d >>= fun () ->
    Lwt_event.sync (Lwt_event.send chan ()) in
  Lwt_event.receive chan

let read ch buf pos len =
  Lwt_event.behavior (fun performed condition evnum ->
    let res = ref `None in
    let fd = Lwt_unix.unix_file_descr ch in
    let poll () =
      try
        Lwt_unix.check_descriptor ch;
        try
          res := `Value (Unix.read fd buf pos len);
          performed := evnum;
          true
        with
          | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> false
      with e ->
        res := `Exn e;
        true in
    let suspend () =
      ignore (Lwt_unix.register_action Lwt_unix.inputs ch (fun () ->
        if !performed < 0
        then
          if poll ()
          then Lwt.wakeup condition ()
          else raise Lwt_unix.Retry_read)) in
    let result () =
      match !res with
        | `None -> invalid_arg "Lwt_event_unix.read"
        | `Value v -> v
        | `Exn e -> raise e in
    { Lwt_event.poll = poll; suspend = suspend; result = result })

let write ch buf pos len =
  Lwt_event.behavior (fun performed condition evnum ->
    let res = ref `None in
    let fd = Lwt_unix.unix_file_descr ch in
    let poll () =
      try
        Lwt_unix.check_descriptor ch;
        try
          res := `Value (Unix.write fd buf pos len);
          performed := evnum;
          true
        with
          | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> false
      with e ->
        res := `Exn e;
        true in
    let suspend () =
      ignore (Lwt_unix.register_action Lwt_unix.outputs ch (fun () ->
        if !performed < 0
        then
          if poll ()
          then Lwt.wakeup condition ()
          else raise Lwt_unix.Retry_write)) in
    let result () =
      match !res with
        | `None -> invalid_arg "Lwt_event_unix.write"
        | `Value v -> v
        | `Exn e -> raise e in
    { Lwt_event.poll = poll; suspend = suspend; result = result })
