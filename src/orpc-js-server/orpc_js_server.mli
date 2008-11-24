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

type obj =
    | Oint of int
    | Ofloat of float
    | Ostring of string
    | Oblock of int * obj array

val to_unit : obj -> unit
val to_int : obj -> int
val to_int32 : obj -> Int32.t
val to_int64 : obj -> Int64.t
val to_float : obj -> float
val to_bool : obj -> bool
val to_char : obj -> char
val to_string : obj -> string

val to_list : (obj -> 'a) -> obj -> 'a list
val to_option : (obj -> 'a) -> obj -> 'a option

val handler : (string * (obj -> Obj.t)) list -> (Netcgi_types.cgi_activation -> unit)
val service : (Netcgi_types.cgi_activation -> unit) -> Netcgi_types.cgi_activation Nethttpd_services.dynamic_service
