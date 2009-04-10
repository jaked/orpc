(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

let (>>=) = Lwt.(>>=)

exception Timeout

module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type S =
sig
  type elt
  type t
  val create : unit -> t
  val add : elt -> t -> unit Lwt.t
  val take : ?timeout:float -> t -> elt Lwt.t
  val size : t -> int
  val fold: ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module Make(Ord: OrderedType) : S with type elt = Ord.t =
struct
  module PQ = Pqueue.Make(Ord)

  type elt = Ord.t
  type t = { m : Lwt_mutex.t; c : Lwt_condition.t; mutable pq : PQ.t }

  let create () = { m = Lwt_mutex.create (); c = Lwt_condition.create (); pq = PQ.empty }

  let add e t =
    Lwt_mutex.lock t.m >>= fun () ->
      t.pq <- PQ.add e t.pq;
      Lwt_condition.signal t.c;
      Lwt_mutex.unlock t.m;
      Lwt.return ()

  let take ?(timeout=(-1.)) t =
    let timed_out = ref false in
    if timeout >= 0.
    then
      Lwt.ignore_result
        (Lwt_unix.sleep timeout >>= fun () ->
          Lwt_mutex.lock t.m >>= fun () ->
            timed_out := true;
            Lwt_condition.signal t.c;
            Lwt_mutex.unlock t.m;
            Lwt.return ());
    Lwt_mutex.lock t.m >>= fun () ->
      let rec while_empty () =
        if !timed_out then Lwt.return false
        else if not (PQ.is_empty t.pq) then Lwt.return true
        else Lwt_condition.wait t.c t.m >>= while_empty in
      while_empty () >>= fun not_empty ->
        let e =
          if not_empty
          then let e = PQ.find_min t.pq in (t.pq <- PQ.remove_min t.pq; Some e)
          else None in
        Lwt_condition.signal t.c;
        Lwt_mutex.unlock t.m;
        match e with Some e -> Lwt.return e | _ -> Lwt.fail Timeout

  let size t = PQ.size t.pq

  let fold f b t = PQ.fold f b t.pq
end
