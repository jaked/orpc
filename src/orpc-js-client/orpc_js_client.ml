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

class type console =
object
  method log : string -> unit
end

let console : console = Ocamljs.var "console"

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

type t = {
  url : string;
  mutable txn_id : int;
  mutable session_id : string option;
  pending_calls : (int, (unit -> Obj.t) -> unit) Hashtbl.t;
  mutable reqs_in_flight : int;
  mutable procs : (string * (Obj.t -> ((unit -> Obj.t) -> unit) -> unit)) list option;
}

let create url = {
  url = url;
  txn_id = 0;
  session_id = None;
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
  let msg = { m_session_id = t.session_id; msg = msg } in
  let xhr = Dom.new_XMLHttpRequest () in
  xhr#_set_onreadystatechange begin fun () ->
    match xhr#_get_readyState with
      | 4 ->
          t.reqs_in_flight <- t.reqs_in_flight - 1;
          if xhr#_get_status = 200
          then recv t (Obj.obj (unserialize xhr#_get_responseText))
          else begin
            (* if we can't read the msg we don't know the txn_id, so fail all *)
            let r = let s = string_of_int xhr#_get_status ^ xhr#_get_statusText in (fun () -> raise (Failure s)) in
            Hashtbl.iter (fun _ f -> try f r with e -> console#log (Obj.magic e)) t.pending_calls;
            Hashtbl.clear t.pending_calls
          end;
          if t.procs <> None && t.reqs_in_flight = 0 then poll t
      | _ -> ()
  end;
  xhr#open_ "POST" t.url true;
  xhr#setRequestHeader "Content-Type" "text/plain; charset=utf-8";
  xhr#send (serialize (Obj.repr msg));
  t.reqs_in_flight <- t.reqs_in_flight + 1

and recv t msg =
  begin match msg.m_session_id with
    | None -> console#log "got no session id"
    | Some id -> console#log ("got session id " ^ id); t.session_id <- msg.m_session_id
  end;
  match msg.msg with
    | Noop -> console#log "got Noop"
    | Call (txn_id, proc, arg) ->
        console#log (Printf.sprintf "got Call (%d, %s, _)" txn_id proc);
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
        console#log (Printf.sprintf "got Res (%d, _)" txn_id);
        begin
          let call = try Some (Hashtbl.find t.pending_calls txn_id) with Not_found -> None in
          match call with
            | None -> ()
            | Some call ->
                Hashtbl.remove t.pending_calls txn_id;
                call (fun () -> o)
        end
    | Fail (txn_id, s) ->
        console#log (Printf.sprintf "got Fail (%d, _)" txn_id);
        begin
          let call = try Some (Hashtbl.find t.pending_calls txn_id) with Not_found -> None in
          match call with
            | None -> ()
            | Some call ->
                Hashtbl.remove t.pending_calls txn_id;
                call (fun () -> raise (Failure s))
        end

and poll t = ignore (Dom.window#setTimeout (fun () -> send t Noop) 0.)

let call t proc arg pass_reply =
  let txn_id = t.txn_id in
  t.txn_id <- t.txn_id + 1;
  Hashtbl.replace t.pending_calls txn_id pass_reply;
  send t (Call (txn_id, proc, arg))

let bind t procs =
  t.procs <- Some procs;
  poll t
