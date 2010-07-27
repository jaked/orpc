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

type msg =
  | Call of int * string * Obj.t
  | Res of int * Obj.t
  | Fail of int * string

type msgs = {
  m_session_id : string option;
  msgs : msg array;
}

type t = {
  url : string;
  mutable txn_id : int;
  mutable session_id : string option;
  mutable procs : (string * (Obj.t -> ((unit -> Obj.t) -> unit) -> unit)) list option;
  pending_calls : (int, (unit -> Obj.t) -> unit) Hashtbl.t;
  mutable queued_msgs : msg list;
  mutable req_in_flight : bool;
}

let create url = {
  url = url;
  txn_id = 0;
  session_id = None;
  procs = None;
  pending_calls = Hashtbl.create 17;
  queued_msgs = [];
  req_in_flight = false;
}

let rec req t =
  if not t.req_in_flight && t.queued_msgs <> []
  then
    let msgs = { m_session_id = t.session_id; msgs = Array.of_list (List.rev t.queued_msgs); } in
    t.queued_msgs <- [];
    let xhr = Dom.new_XMLHttpRequest () in
    xhr#_set_onreadystatechange begin fun () ->
      match xhr#_get_readyState with
        | 4 ->
            xhr#_set_onreadystatechange ignore;
            t.req_in_flight <- false;
            recv t xhr;
            req t
      | _ -> ()
  end;
  xhr#open_ "POST" t.url true;
  xhr#setRequestHeader "Content-Type" "text/plain; charset=utf-8";
  xhr#send (serialize (Obj.repr msgs));
  t.req_in_flight <- true

and poll ?on_connect t =
  let xhr = Dom.new_XMLHttpRequest () in
  xhr#_set_onreadystatechange begin fun () ->
    match xhr#_get_readyState with
      | 4 ->
          xhr#_set_onreadystatechange ignore;
          recv ?on_connect t xhr;
          poll t
      | _ -> ()
  end;
  let url = t.url ^ "?nonce=" ^ string_of_float (Javascript.new_Date ())#getTime in
  let url =
    match t.session_id with
      | None -> url
      | Some session_id -> url ^ "&session_id=" ^ session_id in
  xhr#open_ "GET" url true;
  xhr#send (Ocamljs.null ());

and send t msg =
  t.queued_msgs <- msg :: t.queued_msgs;
  req t

and recv ?on_connect t xhr =
  if xhr#_get_status <> 200
  then begin
    (* don't know the txn_ids, so fail all *)
    let r = let s = string_of_int xhr#_get_status ^ xhr#_get_statusText in (fun () -> raise (Failure s)) in
    Hashtbl.iter (fun _ f -> try f r with e -> ()) t.pending_calls;
    Hashtbl.clear t.pending_calls;
    match on_connect with
      | None -> ()
      | Some f -> f r
  end
  else
    let msgs = Obj.obj (unserialize xhr#_get_responseText) in

    begin match msgs.m_session_id with
      | None -> ()
      | Some _ as id ->
          match t.session_id with
            | Some _ -> t.session_id <- id
            | None ->
                t.session_id <- id;
                match on_connect with
                  | None -> ()
                  | Some f -> f (fun () -> ())
    end;

    Array.iter
      (function
         | Call (txn_id, proc, arg) ->
             begin
               let proc =
                 match t.procs with
                   | None -> None
                   | Some procs -> try Some (List.assoc proc procs) with Not_found -> None in
               match proc with
                 | None -> send t (Fail (txn_id, Printexc.to_string (Invalid_argument "bad proc")))
                 | Some proc ->
                     proc arg begin fun r ->
                       let reply =
                         try Res (txn_id, r ())
                         with e -> Fail (txn_id, Printexc.to_string e) in
                       send t reply
                     end
             end
         | Res (txn_id, o) ->
             begin
               let call = try Some (Hashtbl.find t.pending_calls txn_id) with Not_found -> None in
               match call with
                 | None -> ()
                 | Some call ->
                     Hashtbl.remove t.pending_calls txn_id;
                     call (fun () -> o)
             end
         | Fail (txn_id, s) ->
             begin
               let call = try Some (Hashtbl.find t.pending_calls txn_id) with Not_found -> None in
               match call with
                 | None -> ()
                 | Some call ->
                     Hashtbl.remove t.pending_calls txn_id;
                     call (fun () -> raise (Failure s))
             end)
      msgs.msgs

let call t proc arg pass_reply =
  let txn_id = t.txn_id in
  t.txn_id <- t.txn_id + 1;
  Hashtbl.replace t.pending_calls txn_id pass_reply;
  send t (Call (txn_id, proc, arg))

let bind t procs =
  t.procs <- Some procs

let connect t on_connect =
  poll ~on_connect t
