let (>>=) = Lwt.bind

open Lwt_event

type 'a t = {
  inch: 'a channel;
  ouch: 'a channel;
}

let create () =
  let q = Queue.create () in
  let inch = new_channel () in
  let ouch = new_channel () in

  let add =
    wrap (receive inch) (fun e ->
      Queue.add e q;
      Lwt.return ()) in

  let take () =
    wrap (send ouch (Queue.peek q)) (fun () ->
      ignore (Queue.take q);
      Lwt.return ()) in

  let rec loop () =
    let evs =
      if Queue.is_empty q
      then [ add ]
      else [ add; take () ] in
    select evs >>= loop in
  ignore (loop ());

  { inch = inch; ouch = ouch }

let add e t = sync (send t.inch e)

let take t = receive t.ouch
