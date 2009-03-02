exception Event_system_not_set
exception Shutdown

val set_event_system : Unixqueue.event_system -> unit
val event_system : unit -> Unixqueue.event_system
val unset_event_system : unit -> unit
