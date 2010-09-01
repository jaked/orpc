(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Copyright (C) 2010 Jacob Donham
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


type t
type session

val create : unit -> t

val bind : t -> (string * (Orpc_js_server.obj -> Orpc_js_server.obj Lwt.t)) list -> unit

val call : t -> session -> string -> Orpc_js_server.obj -> Orpc_js_server.obj Lwt.t

val find_session_by_id : t -> string -> session
val session_id : session -> string
val iter_sessions : t -> (string -> session -> unit) -> unit

val callback : t -> (Cohttpserver.Http_daemon.conn_id -> Cohttp.Http_request.request -> string Lwt_stream.t Lwt.t)
val conn_closed : t -> (Cohttpserver.Http_daemon.conn_id -> unit)
