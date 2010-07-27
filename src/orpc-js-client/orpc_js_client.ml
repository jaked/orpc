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
  sync : bool;
}

type t = {
  transport : [ `Xhr | `Xhr_long_poll ];
  url : string;
  mutable txn_id : int;
  mutable session_id : string option;
  mutable procs : (string * (Obj.t -> Obj.t Lwt.t)) list option;
  pending_calls : (int, Obj.t Lwt.u) Hashtbl.t;
  mutable queued_msgs : msg list;
  mutable req_in_flight : bool;
}

let create ?(transport=`Xhr) url = {
  transport = transport;
  url = url;
  txn_id = 0;
  session_id = None;
  procs = None;
  pending_calls = Hashtbl.create 17;
  queued_msgs = [];
  req_in_flight = false;
}

let rec req c =
  if not c.req_in_flight && c.queued_msgs <> []
  then
    let msgs = { m_session_id = c.session_id; msgs = Array.of_list (List.rev c.queued_msgs); sync = c.transport = `Xhr; } in
    c.queued_msgs <- [];
    let xhr = Dom.new_XMLHttpRequest () in
    xhr#_set_onreadystatechange begin fun () ->
      match xhr#_get_readyState with
        | 4 ->
            xhr#_set_onreadystatechange ignore;
            c.req_in_flight <- false;
            ignore (Dom.window#setTimeout (fun () -> recv c xhr; req c) 0.)
      | _ -> ()
  end;
  xhr#open_ "POST" c.url true;
  xhr#setRequestHeader "Content-Type" "text/plain; charset=utf-8";
  xhr#send (serialize (Obj.repr msgs));
  c.req_in_flight <- true

and poll ?on_connect c =
  let xhr = Dom.new_XMLHttpRequest () in
  xhr#_set_onreadystatechange begin fun () ->
    match xhr#_get_readyState with
      | 4 ->
          xhr#_set_onreadystatechange ignore;
          ignore (Dom.window#setTimeout (fun () -> recv ?on_connect c xhr; poll c) 0.)
      | _ -> ()
  end;
  let url = c.url ^ "?nonce=" ^ string_of_float (Javascript.new_Date ())#getTime in
  let url =
    match c.session_id with
      | None -> url
      | Some session_id -> url ^ "&session_id=" ^ session_id in
  xhr#open_ "GET" url true;
  xhr#send (Ocamljs.null ());

and send c msg =
  c.queued_msgs <- msg :: c.queued_msgs;
  req c

and recv ?on_connect c xhr =
  if xhr#_get_status <> 200
  then begin
    (* don't know the txn_ids, so fail all *)
    let e = Failure (string_of_int xhr#_get_status) in
    Hashtbl.iter (fun _ u -> Lwt.wakeup_exn u e) c.pending_calls;
    Hashtbl.clear c.pending_calls;
    match on_connect with
      | None -> ()
      | Some u -> Lwt.wakeup_exn u e
  end
  else
    let msgs = Obj.obj (unserialize xhr#_get_responseText) in
    handle_msgs ?on_connect c msgs

and handle_msgs ?on_connect c msgs =
  begin match msgs.m_session_id with
    | None -> ()
    | Some _ as id ->
        match c.session_id with
          | Some _ -> c.session_id <- id
          | None ->
              c.session_id <- id;
              match on_connect with
                | None -> ()
                | Some u -> Lwt.wakeup u ()
  end;

  let handle_msg = function
    | Call (txn_id, proc, arg) ->
        begin
          let proc =
            match c.procs with
              | None -> None
              | Some procs -> try Some (List.assoc proc procs) with Not_found -> None in
          match proc with
            | None -> send c (Fail (txn_id, Printexc.to_string (Invalid_argument "bad proc")))
            | Some proc ->
                ignore
                  (lwt reply =
                     Lwt.try_bind
                       (fun () -> proc arg)
                       (fun r -> Lwt.return (Res (txn_id, r)))
                       (fun e -> Lwt.return (Fail (txn_id, Printexc.to_string e))) in
                   send c reply;
                   Lwt.return ())
        end
    | Res (txn_id, o) ->
        begin
          let call = try Some (Hashtbl.find c.pending_calls txn_id) with Not_found -> None in
          match call with
            | None -> ()
            | Some u ->
                Hashtbl.remove c.pending_calls txn_id;
                Lwt.wakeup u o
        end
    | Fail (txn_id, s) ->
        begin
          let call = try Some (Hashtbl.find c.pending_calls txn_id) with Not_found -> None in
          match call with
            | None -> ()
            | Some u ->
                Hashtbl.remove c.pending_calls txn_id;
                Lwt.wakeup_exn u (Failure s)
        end in

  Array.iter handle_msg msgs.msgs

let call c proc arg =
  let t, u = Lwt.wait () in
  let txn_id = c.txn_id in
  c.txn_id <- c.txn_id + 1;
  Hashtbl.replace c.pending_calls txn_id u;
  send c (Call (txn_id, proc, arg));
  t

let bind c procs =
  match c.transport with
    | `Xhr -> raise (Failure "bind not supported for `Xhr transport");
    | _ -> c.procs <- Some procs

let connect c =
  match c.transport with
    | `Xhr -> raise (Failure "connect not supported for `Xhr transport");
    | _ ->
        let t, u = Lwt.wait () in
        poll ~on_connect:u c;
        t
