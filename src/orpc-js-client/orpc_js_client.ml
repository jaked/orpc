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

let serialize o =
  let a = Javascript.new_Array () in
  let push o = ignore (a#push o) in
  let push_ffff o = push o; push (Obj.repr (Javascript.Js_string.fromCharCode 65535)) in
  let rec loop o = (* XXX maybe keep an explicit stack here? *)
    match Javascript.typeof o with
      | "string" -> push (Obj.repr "s"); push_ffff o
      | "number" -> push_ffff o
      | "boolean" -> push_ffff (Obj.repr (if Obj.obj o then 1 else 0))
      | "object" -> (* XXX check for Array *)
          push (Obj.repr "[");
          let s = Obj.size o - 1 in
          for i = 0 to s do loop (Obj.field o i) done;
          push_ffff (Obj.repr (Obj.tag o));
          push (Obj.repr "]")
      | _ -> raise (Failure "serialize: unserializeable heap object") in
  loop o;
  a#join ""

(* this is in dom package but don't want dependency *)
class type xMLHttpRequest =
object
  method _set_onreadystatechange : (unit -> unit) -> unit
  method _get_readyState : int
  (* method _get_responseXML : Dom.document ? *)
  method _get_responseText : string
  method _get_status : int
  method _get_statusText : string
  method abort : unit
  method getAllResponseHeaders : string
  method getResponseHeader : string -> string
  method open__ : string -> string -> bool -> unit
  method send : string -> unit
  method setRequestHeader : string -> string -> unit
end

external new_XMLHttpRequest : unit -> xMLHttpRequest = "$new" "XMLHttpRequest"

type t = string

let create url = url

let sync_call url proc arg =
  let xhr = new_XMLHttpRequest () in
  xhr#open__ "POST" url false;
  xhr#setRequestHeader "Content-Type" "text/plain";
  xhr#send (serialize (Obj.repr (proc, arg)));
  if xhr#_get_status = 200
  then Javascript.eval xhr#_get_responseText
  else raise (Failure xhr#_get_statusText)

let add_call url proc arg pass_reply =
  let xhr = new_XMLHttpRequest () in
  xhr#_set_onreadystatechange (fun () ->
    match xhr#_get_readyState with
      | 4 ->
          let r =
            if xhr#_get_status = 200
            then let o = Javascript.eval xhr#_get_responseText in (fun () -> o)
            else let s = xhr#_get_statusText in (fun () -> raise (Failure s)) in
          pass_reply r
      | _ -> ());
  xhr#open__ "POST" url true;
  xhr#setRequestHeader "Content-Type" "text/plain; charset=utf-8";
  xhr#send (serialize (Obj.repr (proc, arg)))
