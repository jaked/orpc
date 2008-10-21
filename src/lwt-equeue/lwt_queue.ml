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

let (>>=) = Lwt.(>>=)

type 'a t = { m : Lwt_mutex.t; c : Lwt_condition.t; q : 'a Queue.t }

let create () = { m = Lwt_mutex.create (); c = Lwt_condition.create (); q = Queue.create () }

let add e t =
  Lwt_mutex.lock t.m >>= fun () ->
    Queue.add e t.q;
    Lwt_condition.signal t.c;
    Lwt_mutex.unlock t.m;
    Lwt.return ()

let take t =
  Lwt_mutex.lock t.m >>= fun () ->
    let rec while_empty () =
      if Queue.is_empty t.q
      then Lwt_condition.wait t.c t.m >>= while_empty
      else Lwt.return () in
    while_empty () >>= fun () ->
      let e = Queue.take t.q in
      Lwt_condition.signal t.c;
      Lwt_mutex.unlock t.m;
      Lwt.return e
