(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Copyright (C) 2010 Jacob Donham
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

val x_char : Xdr.xdr_type_term
val x_list : Xdr.xdr_type_term -> Xdr.xdr_type_term
val to_list : (Xdr.xdr_value -> 'a) -> Xdr.xdr_value -> 'a list
val of_list : ('a -> Xdr.xdr_value) -> 'a list -> Xdr.xdr_value
val to_option : (Xdr.xdr_value -> 'a) -> Xdr.xdr_value -> 'a option
val of_option : ('a -> Xdr.xdr_value) -> 'a option -> Xdr.xdr_value

val orpc_aux_to_orpc_result : (Xdr.xdr_value -> 'a) -> (Xdr.xdr_value -> exn) -> Xdr.xdr_value -> ('a, exn) Orpc.orpc_result
val orpc_aux_of_orpc_result : ('a -> Xdr.xdr_value) -> (exn -> Xdr.xdr_value) -> ('a, exn) Orpc.orpc_result -> Xdr.xdr_value
val orpc_aux_xdr_orpc_result : Xdr.xdr_type_term -> Xdr.xdr_type_term -> Xdr.xdr_type_term;;

(* a place to stash the session on asynchronous calls. not thread safe, obviously. *)
val session : Rpc_server.session option ref
