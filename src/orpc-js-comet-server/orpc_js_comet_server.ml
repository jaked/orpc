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

open Cohttp
open Cohttpserver

type req = {
  conn_id : Http_daemon.conn_id;
  reply : string Lwt_stream.t Lwt.u;
}

type session = {
  session_id : string;
  mutable txn_id : int;
  pending_calls : (int, Orpc_js_server.obj Lwt.u) Hashtbl.t;
  mutable queued_msgs : Orpc_js_server.msg list;
  mutable req : req option;
}

type t = {
  sessions_by_session_id : (string, session) Hashtbl.t;
  sessions_by_conn_id : (Http_daemon.conn_id, session) Hashtbl.t;
  (* XXX expire sessions *)

  mutable procs : (string * (Orpc_js_server.obj -> Orpc_js_server.obj Lwt.t)) list option;
}

let create () = {
  sessions_by_session_id = Hashtbl.create 17;
  sessions_by_conn_id = Hashtbl.create 17;
  procs = None;
}

let bind s procs = s.procs <- Some procs

let reply sess =
  (* XXX don't send session_id if req already had the right one *)
  let msgs = { Orpc_js_server.m_session_id = Some sess.session_id; msgs = Array.of_list (List.rev sess.queued_msgs); sync = false; } in
  sess.queued_msgs <- [];
  let body = Orpc_js_server.string_of_msgs msgs in
  Http_daemon.respond ~body ()

let send s sess msg =
  sess.queued_msgs <- msg :: sess.queued_msgs;
  match sess.req with
    | None -> ()
    | Some req ->
        sess.req <- None;
        Hashtbl.remove s.sessions_by_conn_id req.conn_id;
        ignore
          (lwt reply = reply sess in
           Lwt.wakeup req.reply reply;
           Lwt.return ())

let call s sess proc arg =
  let txn_id = sess.txn_id in
  sess.txn_id <- sess.txn_id + 1;
  let t, u = Lwt.wait () in
  Hashtbl.add sess.pending_calls txn_id u;
  send s sess (Orpc_js_server.Call (txn_id, proc, arg));
  t

let get s conn_id req =
  let session_id =
    try Some (Http_request.param req "session_id")
    with Http_types.Param_not_found _ -> None in

  let sess =
    match session_id with
      | None -> None
      | Some session_id ->
          try Some (Hashtbl.find s.sessions_by_session_id session_id)
          with Not_found -> None in

  match sess with
    | None ->
        let sess = {
          session_id = string_of_int (Random.int max_int); (* XXX *)
          txn_id = 0;
          pending_calls = Hashtbl.create 17;
          queued_msgs = [];
          req = None;
        } in
        Hashtbl.replace s.sessions_by_session_id sess.session_id sess;
        reply sess (* return session_id *)
    | Some sess ->
        begin match sess.req with
          | None -> ()
          | Some req ->
              ignore
                (lwt reply = Http_daemon.respond_error ~status:`Bad_request ~body:"additional connection" () in
                 Lwt.wakeup req.reply reply;
                 Lwt.return ());
              sess.req <- None;
              Hashtbl.remove s.sessions_by_conn_id req.conn_id
        end;
        if sess.queued_msgs <> []
        then reply sess
        else
          let t, u = Lwt.wait () in
          sess.req <- Some { reply = u; conn_id = conn_id };
          Hashtbl.replace s.sessions_by_conn_id conn_id sess;
          t

let post s req =
  let body = Http_request.body req in
  lwt body_string = Http_message.string_of_body body in
  let msgs = Orpc_js_server.msgs_of_string body_string in

  let sess =
    match msgs.Orpc_js_server.m_session_id with
      | None -> None
      | Some session_id ->
          try Some (Hashtbl.find s.sessions_by_session_id session_id)
          with Not_found -> None in

  match sess with
    | None -> Http_daemon.respond_error ~status:`Bad_request ~body:"session_id required" ()
    | Some sess ->
        Array.iter
          (function
             | Orpc_js_server.Call (txn_id, proc, arg) ->
                 begin
                   let proc =
                     match s.procs with
                       | None -> None
                       | Some procs -> try Some (List.assoc proc procs) with Not_found -> None in
                   match proc with
                     | None -> send s sess (Orpc_js_server.Fail (txn_id, Printexc.to_string (Invalid_argument "bad proc")))
                     | Some proc ->
                         ignore
                           (lwt reply =
                              Lwt.catch
                                (fun () -> lwt res = proc arg in Lwt.return (Orpc_js_server.Res (txn_id, res)))
                                (fun e -> Lwt.return (Orpc_js_server.Fail (txn_id, Printexc.to_string e))) in
                            send s sess reply;
                          Lwt.return ())
                 end
             | Orpc_js_server.Res (txn_id, res) ->
                 begin
                   try
                     Lwt.wakeup (Hashtbl.find sess.pending_calls txn_id) res;
                     Hashtbl.remove sess.pending_calls txn_id
                   with Not_found -> ()
                 end
             | Orpc_js_server.Fail (txn_id, s) ->
                 begin
                   try
                     Lwt.wakeup_exn (Hashtbl.find sess.pending_calls txn_id) (Failure s);
                     Hashtbl.remove sess.pending_calls txn_id
                   with Not_found -> ()
                 end)
          msgs.Orpc_js_server.msgs;

        (* XXX to potentially save a poll request, defer replies until here *)
        (* XXX and/or param to wait a bit before reply? *)
        reply sess

let callback s conn_id req =
  match Http_request.meth req with
    | `GET -> get s conn_id req
    | `POST -> post s req
    | _ -> Http_daemon.respond_error ~status:`Method_not_allowed ~body:"method not allowed" ()

let conn_closed s conn_id =
  try
    let sess = Hashtbl.find s.sessions_by_conn_id conn_id in
    match sess.req with
      | Some req when req.conn_id = conn_id ->
          sess.req <- None;
          Hashtbl.remove s.sessions_by_conn_id conn_id
      | _ -> ()
  with Not_found -> ()

let session_id sess = sess.session_id

let find_session_by_id s id = Hashtbl.find s.sessions_by_session_id id

let iter_sessions s f = Hashtbl.iter f s.sessions_by_session_id
