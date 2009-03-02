exception Event_system_not_set
exception Shutdown

let event_system = ref (fun () -> raise Event_system_not_set)

let set_event_system es = event_system := fun () -> es

let unset_event_system () =
  let es = !event_system () in
  es#add_event (Unixqueue.Extra Shutdown);
  event_system := fun () -> raise Event_system_not_set

let event_system () = !event_system ()
