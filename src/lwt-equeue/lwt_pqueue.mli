(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008 Skydeck, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA
 *)

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
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module Make(Ord: OrderedType) : S with type elt = Ord.t
