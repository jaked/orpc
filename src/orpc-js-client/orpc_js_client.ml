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

let serialize o =
  let a = Javascript.new_Array () in
  let push o = ignore (a#push o) in
  let push_ffff o = push o; push (Obj.repr (Javascript.Js_string.fromCharCode 65535)) in
  let rec loop o = (* XXX maybe keep an explicit stack here? *)
    match Javascript.typeof o with
      | "string" -> push (Obj.repr "s"); push_ffff o
      | "number" -> push_ffff o
      | "boolean" -> push (Obj.repr (if Obj.obj o then "t" else "f"))
      | "object" -> (* XXX check for Array *)
          push (Obj.repr "[");
          let s = Obj.size o - 1 in
          for i = 0 to s do loop (Obj.field o i) done;
          push_ffff (Obj.repr (Obj.tag o));
          push (Obj.repr "]")
      | _ -> raise (Failure "serialize: unserializeable heap object") in
  loop o;
  a#join ""

let unserialize = Javascript.eval

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

type t = {
  url : string;
  mutable txn_id : int;
  mutable session_id : string;
  pending_calls : (int, (unit -> Obj.t) -> unit) Hashtbl.t;
  mutable reqs_in_flight : int;
  mutable procs : (string, Obj.t -> ((unit -> Obj.t) -> unit) -> unit) Hashtbl.t option;
}

let create url = {
  url = url;
  txn_id = 0;
  session_id = "";
  pending_calls = Hashtbl.create 17;
  reqs_in_flight = 0;
  procs = None;
}

type msg_t =
  | Noop
  | Call of int * string * Obj.t
  | Res of int * Obj.t
  | Fail of int * string

type msg = {
  m_session_id : string option;
  msg : msg_t;
}

let rec send t msg =
  let msg = { m_session_id = Some t.session_id; msg = msg } in
  let xhr = new_XMLHttpRequest () in
  xhr#_set_onreadystatechange begin fun () ->
    match xhr#_get_readyState with
      | 4 ->
          t.reqs_in_flight <- t.reqs_in_flight - 1;
          if xhr#_get_status = 200
          then recv t (Obj.obj (unserialize xhr#_get_responseText))
          else begin
            (* if we can't read the msg we don't know the txn_id, so fail all *)
            let r = let s = xhr#_get_statusText in (fun () -> raise (Failure s)) in
            Hashtbl.iter (fun _ f -> f r) t.pending_calls;
            Hashtbl.clear t.pending_calls
          end;
          if t.procs <> None && t.reqs_in_flight = 0 then poll t
      | _ -> ()
  end;
  xhr#open__ "POST" t.url true;
  xhr#setRequestHeader "Content-Type" "text/plain; charset=utf-8";
  xhr#send (serialize (Obj.repr msg));
  t.reqs_in_flight <- t.reqs_in_flight + 1

and recv t msg =
  begin match msg.m_session_id with
    | None -> ()
    | Some s -> t.session_id <- s
  end;
  match msg.msg with
    | Noop -> ()
    | Call (txn_id, proc, arg) ->
        begin
          match t.procs with
            | None -> ()
            | Some procs ->
                try
                  Hashtbl.find procs proc arg begin fun r ->
                    try send t (Res (txn_id, r ()))
                    with e -> send t (Fail (txn_id, Printexc.to_string e))
                  end
                with Not_found -> ()
        end
    | Res (txn_id, o) ->
        begin
          try
            Hashtbl.find t.pending_calls txn_id (fun () -> o);
            Hashtbl.remove t.pending_calls txn_id
          with Not_found -> ()
        end
    | Fail (txn_id, s) ->
        begin
          try
            Hashtbl.find t.pending_calls txn_id (fun () -> raise (Failure s));
            Hashtbl.remove t.pending_calls txn_id
          with Not_found -> ()
        end

and poll t = send t Noop

let call t proc arg pass_reply =
  let txn_id = t.txn_id in
  t.txn_id <- t.txn_id + 1;
  Hashtbl.replace t.pending_calls txn_id pass_reply;
  send t (Call (txn_id, proc, arg))

let bind t procs =
  let h = Hashtbl.create (List.length procs * 2) in
  List.iter (fun (k, v) -> Hashtbl.replace h k v) procs;
  t.procs <- Some h;
  poll t
