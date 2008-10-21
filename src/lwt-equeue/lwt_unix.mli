(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008 Skydeck, Inc
 * Original file (lwt_unix.mli in the Lwt source distribution) is
 * Copyright (C) 2005-2008 Jérôme Vouillon
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

exception Event_system_not_set
val set_event_system : Unixqueue.event_system -> unit
val unset_event_system : unit -> unit

(** Module [Lwt_unix]: thread-compatible system calls *)

val sleep : float -> unit Lwt.t
      (** [sleep d] is a threads which remain suspended for [d] seconds
          (letting other threads run) and then terminates. *)
val yield : unit -> unit Lwt.t
      (** [yield ()] is a threads which suspends itself (letting other
          thread run) and then resumes as soon as possible and
          terminates. *)

(****)

(** These functions behave as their [Unix] counterparts, but let other
    threads run while waiting for the completion of the system call. *)

type file_descr

val read : file_descr -> string -> int -> int -> int Lwt.t
val write : file_descr -> string -> int -> int -> int Lwt.t

val wait_read : file_descr -> unit Lwt.t
(** waits (without blocking other threads)
    until there is something to read on the file descriptor *)

val wait_write : file_descr -> unit Lwt.t
(** waits (without blocking other threads)
    until it is possible to write on the file descriptor *)

val pipe : unit -> file_descr * file_descr
val pipe_in : unit -> file_descr * Unix.file_descr
val pipe_out : unit -> Unix.file_descr * file_descr
val socket :
  Unix.socket_domain -> Unix.socket_type -> int -> file_descr
val socketpair :
  Unix.socket_domain -> Unix.socket_type -> int -> file_descr * file_descr
val bind : file_descr -> Unix.sockaddr -> unit
val listen : file_descr -> int -> unit
val accept : file_descr -> (file_descr * Unix.sockaddr) Lwt.t
val connect : file_descr -> Unix.sockaddr -> unit Lwt.t
val shutdown : file_descr -> Unix.shutdown_command -> unit
val close : file_descr -> unit

val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
val set_close_on_exec : file_descr -> unit

(****)

(** Aborting a connection *)

val abort : file_descr -> exn -> unit
      (** Makes all current and further uses of the file descriptor
          fail with the given exception *)

(****)

(** File descriptor wrappings/unwrappings *)

(* [of_unix_file_descr] has the side-effect of putting the file
   descriptor in non-blocking mode. *)

val unix_file_descr : file_descr -> Unix.file_descr
val of_unix_file_descr : Unix.file_descr -> file_descr

(****)

(*XXX*)
(*
val open_process_in: string -> Lwt_chan.in_channel Lwt.t
val open_process_out: string -> out_channel Lwt.t
val open_process: string -> (in_channel * out_channel) Lwt.t
val open_process_full:
  string -> string array ->
  (in_channel * out_channel * in_channel) Lwt.t
val close_process_in: in_channel -> Unix.process_status Lwt.t
val close_process_out: out_channel -> Unix.process_status Lwt.t
val close_process:
  in_channel * out_channel -> Unix.process_status Lwt.t
val close_process_full:
  in_channel * out_channel * in_channel ->
  Unix.process_status Lwt.t
*)
