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

type ('a, 'b) orpc_result = Orpc_success of 'a | Orpc_failure of exn

val pack_orpc_result : (unit -> 'a) -> ('a, exn) orpc_result
val pack_orpc_result_async : (('a -> unit) -> unit) -> (('a, exn) orpc_result -> unit) -> unit
val unpack_orpc_result : ('a, exn) orpc_result -> 'a
