open Cohttp
open Cohttpserver

let server = Orpc_js_comet_server.create ()

module Server =
struct
  let n = ref 0

  let clicks () = Lwt.return !n

  let click () =
    incr n;
    Orpc_js_comet_server.iter_sessions server
      (fun _ sess ->
         let module M = Comet_js_comet_srv.Lwt(struct let server = server let session = sess end) in
         ignore (M.set_clicks !n));
    Lwt.return ()
end

let _ = let module M = Proto_js_srv.Lwt(Server) in Orpc_js_comet_server.bind server M.funcs

let callback conn_id req out =
  match Http_request.path req with
    | "/" -> Http_daemon.respond_file ~fname:"index.html" ~mime_type:"text/html" out
    | "/_build/clicks.js" -> Http_daemon.respond_file ~fname:"_build/clicks.js" ~mime_type:"application/javascript" out
    | "/clicks" -> Orpc_js_comet_server.callback server conn_id req out
    | url -> Http_daemon.respond_error ~status:`Not_found ~body:("not found: " ^ url) out

let conn_closed conn_id = Orpc_js_comet_server.conn_closed server conn_id

let exn_handler exn out = Lwt.return ()

let spec = {
  Http_daemon.address = "0.0.0.0";
  auth = `None;
  callback = callback;
  conn_closed = conn_closed;
  port = 9007;
  root_dir = None;
  exn_handler = exn_handler;
  timeout = None;
  auto_close = true;
}

let _ = Lwt_main.run (Http_daemon.main spec)
