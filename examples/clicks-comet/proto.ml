module type Sync =
sig
  val clicks : unit -> int
  val click : unit -> unit
end

module type Lwt =
sig
  val clicks : unit -> int Lwt.t
  val click : unit -> unit Lwt.t
end
