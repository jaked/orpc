val sleep : float -> unit Lwt_event.event

val read : Lwt_unix.file_descr -> string -> int -> int -> int Lwt_event.event
val write : Lwt_unix.file_descr -> string -> int -> int -> int Lwt_event.event
