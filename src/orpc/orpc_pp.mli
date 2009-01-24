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

val pp_array : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit
val pp_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_option : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

module type Trace =
sig
  type t
  val trace_call : string -> (Format.formatter -> unit) -> t
  val trace_reply_ok : t -> (Format.formatter -> unit) -> unit
  val trace_reply_exn : t -> exn -> (Format.formatter -> unit) -> unit
end

module Trace_of_formatter (F : sig val formatter : Format.formatter end) : Trace
