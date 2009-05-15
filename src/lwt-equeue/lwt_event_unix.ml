let (>>=) = Lwt.bind

let sleep d =
  let chan = Lwt_event.new_channel () in
  let _ =
    Lwt_unix.sleep d >>= fun () ->
    Lwt_event.sync (Lwt_event.send chan ()) in
  Lwt_event.receive chan

(*
  XXX

  Lwt_unix.run here doesn't seem good. maybe wrap should be like
  bind. but then these don't mean quite what we want. if someone else
  has gotten the read before us we'd like to not sync yet. I think we
  need to get deeper into Lwt_unix, and call Unix.read /
  register_action ourselves, so we can trap EAGAIN / EWOULDBLOCK
*)

let read ch buf pos len =
  let chan = Lwt_event.new_channel () in
  let _ =
    Lwt_unix.wait_read ch >>= fun () ->
      Lwt_event.sync (Lwt_event.send chan ()) in
  Lwt_event.wrap (Lwt_event.receive chan) (fun () -> Lwt_unix.run (Lwt_unix.read ch buf pos len))

let write ch buf pos len =
  let chan = Lwt_event.new_channel () in
  let _ =
    Lwt_unix.wait_read ch >>= fun () ->
      Lwt_event.sync (Lwt_event.send chan ()) in
  Lwt_event.wrap (Lwt_event.receive chan) (fun () -> Lwt_unix.run (Lwt_unix.write ch buf pos len))
