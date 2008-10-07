module Sync =
struct
  val clicks : unit -> int
  val click : unit -> int
end

module Lwt =
struct
  val clicks : unit -> int Lwt.t
  val click : unit -> int Lwt.t
end
