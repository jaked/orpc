module type Sync =
sig
  val clicks : unit -> int
  val click : unit -> int
end

module type Lwt =
sig
  val clicks : unit -> int Lwt.t
  val click : unit -> int Lwt.t
end
