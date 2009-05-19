type 'a t

val create : unit -> 'a t
val add : 'a -> 'a t -> unit Lwt.t
val take : 'a t -> 'a Lwt_event.event
