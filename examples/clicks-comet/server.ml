open Cohttp
open Cohttpserver

type req = {
  conn_id : Http_daemon.conn_id;
  finished : unit Lwt.u;
  out : Lwt_io.output_channel Lwt.t;
}

type session = {
  session_id : string;
  mutable txn_id : int;
  pending_calls : (int, Orpc_js_server.obj Lwt.u) Hashtbl.t;
  mutable queued_msgs : Orpc_js_server.msg list;
  mutable req : req option;
}

(* XXX expire sessions *)
let sessions_by_session_id = Hashtbl.create 17
let sessions_by_conn_id = Hashtbl.create 17

let reply sess out =
  (* XXX don't send session_id if req already had the right one *)
  let msgs = { Orpc_js_server.m_session_id = Some sess.session_id; msgs = Array.of_list (List.rev sess.queued_msgs); } in
  sess.queued_msgs <- [];
  let body = Orpc_js_server.string_of_msgs msgs in
  Http_daemon.respond ~body out

let send sess msg =
  sess.queued_msgs <- msg :: sess.queued_msgs;
  match sess.req with
    | None -> ()
    | Some req ->
        sess.req <- None;
        Hashtbl.remove sessions_by_conn_id req.conn_id;
        ignore
          (lwt () = reply sess req.out in
           Lwt.wakeup req.finished ();
           Lwt.return ())

module Comet_js_comet_srv (S : sig val session : session end) =
struct
  let set_clicks n =
    let txn_id = S.session.txn_id in
    S.session.txn_id <- S.session.txn_id + 1;
    let t, u = Lwt.wait () in
    Hashtbl.add S.session.pending_calls txn_id u;
    send S.session (Orpc_js_server.Call (txn_id, "set_clicks", Comet_js_aux.of_set_clicks'arg n));
    lwt o = t in
    try Lwt.return (Comet_js_aux.to_set_clicks'res o)
    with e -> Lwt.fail e
end

module Server =
struct
  let n = ref 0

  let clicks () = !n

  let click () =
    incr n;
    Hashtbl.iter
      (fun _ sess ->
         let module M = Comet_js_comet_srv (struct let session = sess end) in
         ignore (M.set_clicks !n))
      sessions_by_session_id
end

let new_session () =
  let sess = {
    session_id = string_of_int (Random.int max_int); (* XXX *)
    txn_id = 0;
    pending_calls = Hashtbl.create 17;
    queued_msgs = [];
    req = None;
  } in
  Hashtbl.replace sessions_by_session_id sess.session_id sess;
  sess

let clicks_get conn_id req out =
  let session_id =
    try Some (Http_request.param req "session_id")
    with Http_types.Param_not_found _ -> None in

  let sess =
    match session_id with
      | None -> None
      | Some session_id ->
          try Some (Hashtbl.find sessions_by_session_id session_id)
          with Not_found -> None in

  match sess with
    | None ->
        let sess = new_session () in
        reply sess out (* return session_id *)
    | Some sess ->
        begin match sess.req with
          | None -> ()
          | Some req ->
              ignore
                (lwt () = Http_daemon.respond_error ~status:`Bad_request ~body:"additional connection" out in
                 Lwt.wakeup req.finished ();
                 Lwt.return ());
              sess.req <- None;
              Hashtbl.remove sessions_by_conn_id req.conn_id
        end;
        if sess.queued_msgs <> []
        then reply sess out
        else
          let t, u = Lwt.wait () in
          sess.req <- Some { finished = u; out = out; conn_id = conn_id };
          Hashtbl.replace sessions_by_conn_id conn_id sess;
          t

let clicks_post _ req out =
  let body = Http_request.body req in
  lwt body_string = Http_message.string_of_body body in
  let msgs = Orpc_js_server.msgs_of_string body_string in

  let sess =
    match msgs.Orpc_js_server.m_session_id with
      | None -> None
      | Some session_id ->
          try Some (Hashtbl.find sessions_by_session_id session_id)
          with Not_found -> None in

  match sess with
    | None -> Http_daemon.respond_error ~status:`Bad_request ~body:"session_id required" out
    | Some sess ->
        Array.iter
          (function
             | Orpc_js_server.Call (txn_id, proc, arg) ->
                 let reply =
                   try
                     let proc =
                       match proc with
                         | "click" -> (fun x0 -> Proto_js_aux.of_click'res (Server.click (Proto_js_aux.to_click'arg x0)))
                         | "clicks" -> (fun x0 -> Proto_js_aux.of_clicks'res (Server.clicks (Proto_js_aux.to_clicks'arg x0)))
                         | _ -> raise (Invalid_argument "bad proc") in
                     Orpc_js_server.Res (txn_id, proc arg)
                   with e -> Orpc_js_server.Fail (txn_id, Printexc.to_string e) in
                 send sess reply
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
        reply sess out

let callback conn_id req out =
  match Http_request.path req with
    | "/" -> Http_daemon.respond_file ~fname:"index.html" ~mime_type:"text/html" out
    | "/_build/clicks.js" -> Http_daemon.respond_file ~fname:"_build/clicks.js" ~mime_type:"application/javascript" out
    | "/clicks" ->
        begin match Http_request.meth req with
          | `GET -> clicks_get conn_id req out
          | `POST -> clicks_post conn_id req out
          | _ -> Http_daemon.respond_error ~status:`Method_not_allowed ~body:"method not allowed" out
        end
    | url -> Http_daemon.respond_error ~status:`Not_found ~body:("not found: " ^ url) out

let conn_closed conn_id =
  try
    let sess = Hashtbl.find sessions_by_conn_id conn_id in
    match sess.req with
      | Some req when req.conn_id = conn_id ->
          sess.req <- None;
          Hashtbl.remove sessions_by_conn_id conn_id
      | _ -> ()
  with Not_found -> ()

let exn_handler exn out = Lwt.return ()

let main () =
  (* Http_common.debug := true; *)
  let spec = {
    Http_daemon.address = "192.168.206.129";
    auth = `None;
    callback = callback;
    conn_closed = conn_closed;
    port = 9007;
    root_dir = None;
    exn_handler = exn_handler;
    timeout = None;
    auto_close = true;
  } in
  Lwt_main.run (Http_daemon.main spec)

;;

main ()
