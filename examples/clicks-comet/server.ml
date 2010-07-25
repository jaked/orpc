open Cohttp
open Cohttpserver

type conn = {
  conn_id : Http_daemon.conn_id;
  reqs : (unit Lwt.u * Lwt_io.output_channel Lwt.t) Queue.t;
}

type session = {
  session_id : string;
  mutable txn_id : int;
  pending_calls : (int, Orpc_js_server.obj Lwt.u) Hashtbl.t;
  mutable conn : conn option;
}

let send sess msg =
  (* XXX don't send session_id if req already had the right one *)
  let msg = { Orpc_js_server.m_session_id = Some sess.session_id; msg = msg; } in
  let body = Orpc_js_server.string_of_msg msg in
  match sess.conn with
    | None ->
        Http_common.debug_print ("sending to " ^ sess.session_id ^ ", no conn");
        ()  (* XXX queue send until reconnect or timeout *)
    | Some conn ->
        Http_common.debug_print ("sending to " ^ sess.session_id ^ ", conn_id " ^ Http_daemon.string_of_conn_id conn.conn_id);
        try
          let (u, out) = Queue.take conn.reqs in
          ignore (lwt () = Http_daemon.respond ~body out in Lwt.wakeup u (); Lwt.return ())
        with Queue.Empty ->
          Http_common.debug_print ("no more reqs sending to " ^ sess.session_id ^ ", conn_id " ^ Http_daemon.string_of_conn_id conn.conn_id)

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

(* XXX expire sessions *)
let sessions_by_session_id = Hashtbl.create 17
let sessions_by_conn_id = Hashtbl.create 17

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

let new_session conn_id =
  let sess = {
    txn_id = 0;
    session_id = string_of_int (Random.int max_int); (* XXX *)
    pending_calls = Hashtbl.create 17;
    conn = Some { conn_id = conn_id; reqs = Queue.create (); }
  } in
  Hashtbl.replace sessions_by_session_id sess.session_id sess;
  Hashtbl.replace sessions_by_conn_id conn_id sess;
  sess

let clicks conn_id req out =
  let body = Http_request.body req in
  lwt body_string = Http_message.string_of_body body in
  let msg = Orpc_js_server.msg_of_string body_string in

  let sess =
    match msg.Orpc_js_server.m_session_id with
      | None ->
          begin
            try
              (* additional request before reply with session_id is received? *)
              let sess = Hashtbl.find sessions_by_conn_id conn_id in
              Http_common.debug_print ("no session_id but found conn_id " ^ Http_daemon.string_of_conn_id conn_id);
              sess
            with Not_found ->
              (* new connection, new session *)
              Http_common.debug_print ("no session_id, new session for conn_id " ^ Http_daemon.string_of_conn_id conn_id);
              new_session conn_id
          end
      | Some session_id ->
          begin
            try
              let sess = Hashtbl.find sessions_by_session_id session_id in
              match sess.conn with
                | None ->
                    Http_common.debug_print ("session_id " ^ session_id ^ ", reconnect for conn_id " ^ Http_daemon.string_of_conn_id conn_id);
                    (* conn was closed, this is reconnect *)
                    sess.conn <- Some { conn_id = conn_id; reqs = Queue.create (); };
                    Hashtbl.replace sessions_by_conn_id conn_id sess;
                    sess
                | Some conn when conn.conn_id <> conn_id ->
                    Http_common.debug_print ("session_id " ^ session_id ^ ", extra connect with conn_id " ^ Http_daemon.string_of_conn_id conn_id);
                    (* new connection for session which is already connected. drop the old one *)
                    sess.conn <- Some { conn_id = conn_id; reqs = Queue.create (); };
                    Hashtbl.remove sessions_by_conn_id conn.conn_id;
                    Hashtbl.replace sessions_by_conn_id conn_id sess;
                    sess
                | _ ->
                    Http_common.debug_print ("session_id " ^ session_id ^ ", conn_id " ^ Http_daemon.string_of_conn_id conn_id);
                    (* request on existing connection *)
                    sess
            with Not_found ->
              Http_common.debug_print ("dead session_id " ^ session_id ^ ", new session for conn_id " ^ Http_daemon.string_of_conn_id conn_id);
              (* connection for dead session. *)
              new_session conn_id
          end in

  let t, u = Lwt.wait () in
  begin match sess.conn with
    | None -> assert false
    | Some conn -> Queue.add (u, out) conn.reqs
  end;

  begin match msg.Orpc_js_server.msg with
    | Orpc_js_server.Noop ->
        Http_common.debug_print "got Noop";
        ()
    | Orpc_js_server.Call (txn_id, proc, arg) ->
        Http_common.debug_print "got Call";
        begin
          try
            let proc =
              match proc with
                | "click" -> (fun x0 -> Proto_js_aux.of_click'res (Server.click (Proto_js_aux.to_click'arg x0)))
                | "clicks" -> (fun x0 -> Proto_js_aux.of_clicks'res (Server.clicks (Proto_js_aux.to_clicks'arg x0)))
                | _ -> raise (Invalid_argument "bad proc") in
            try send sess (Orpc_js_server.Res (txn_id, proc arg))
            with e -> send sess (Orpc_js_server.Fail (txn_id, Printexc.to_string e))
          with e -> send sess (Orpc_js_server.Fail (txn_id, Printexc.to_string e))
        end
    | Orpc_js_server.Res (txn_id, res) ->
        Http_common.debug_print "got Res";
        begin
          try
            Lwt.wakeup (Hashtbl.find sess.pending_calls txn_id) res;
            Hashtbl.remove sess.pending_calls txn_id
          with Not_found -> ()
        end
    | Orpc_js_server.Fail (txn_id, s) ->
        Http_common.debug_print "got Fail";
        begin
          try
            Lwt.wakeup_exn (Hashtbl.find sess.pending_calls txn_id) (Failure s);
            Hashtbl.remove sess.pending_calls txn_id
          with Not_found -> ()
        end
  end;
  t

let callback conn_id req out =
  match Http_request.path req with
    | "/" -> Http_daemon.respond_file ~fname:"index.html" ~mime_type:"text/html" out
    | "/_build/clicks.js" -> Http_daemon.respond_file ~fname:"_build/clicks.js" ~mime_type:"application/javascript" out
    | "/clicks" -> clicks conn_id req out
    | url -> Http_daemon.respond ~body:("not found: " ^ url) out

let conn_closed conn_id =
  try
    let sess = Hashtbl.find sessions_by_conn_id conn_id in
    sess.conn <- None;
    Hashtbl.remove sessions_by_conn_id conn_id
  with Not_found -> ()

let exn_handler exn out = Lwt.return ()

let main () =
  Http_common.debug := true;
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
