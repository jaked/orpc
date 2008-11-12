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

let to_list to'a x =
  let rec loop x =
    if Obj.is_int x && Obj.obj x = 0 then []
    else if Obj.tag x = 0 && Obj.size x = 2 then to'a (Obj.field x 0) :: loop (Obj.field x 1)
    else raise (Invalid_argument "list") in
  loop x

let to_option to'a x =
  if Obj.is_int x && Obj.obj x = 0 then None
  else if Obj.tag x = 0 && Obj.size x = 1 then Some (to'a (Obj.field x 0))
  else raise (Invalid_argument "option")

(*
  XXX

  we use Obj.t here because in essence we are transferring the heap
  representation back and forth. unfortunately it is not identical
  between client and server: floats and int32s are unboxed on the
  Javascript side. we have to convert to boxed for regular OCaml, and
  we need types to guide the conversion, so that happens in the to_*
  functions in *_aux.ml.

  maybe in retrospect not such a clever idea.
*)

let serialize o = raise (Failure "unimplemented")

let unserialize s = raise (Failure "unimplemented")

let handler procs (cgi : Netcgi_types.cgi_activation) =
  let o = unserialize (cgi#argument "BODY")#value in
  if Obj.is_int o || Obj.tag o <> 0 || Obj.size o <> 2 then raise (Failure "bad request");
  let (proc_name, arg) = Obj.obj o in
  if Obj.is_int proc_name || Obj.tag proc_name <> Obj.string_tag then raise (Failure "bad request");
  let proc = try List.assoc (Obj.obj proc_name) procs with Not_found -> raise (Failure "bad_request") in
  let res = serialize (proc arg) in

  (* XXX handle gzip *)
  cgi#set_header
    ~content_type:"text/plain; charset=utf-8"
    ~cache:`No_cache
    ();
  cgi#output#output_string res;
  cgi#output#commit_work ()
