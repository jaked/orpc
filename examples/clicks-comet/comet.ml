module type Sync =
sig
  val set_clicks : int -> unit
end

module type Lwt =
sig
  val set_clicks : int -> unit Lwt.t
end
