open Cohttp
open Cohttpserver

module Server =
struct
  let n = ref 0

  let clicks () = Lwt.return !n
  let click () = incr n; Lwt.return !n
end

module M = Proto_js_srv.Lwt(Server)

let clicks req =
  let body = Http_request.body req in
  lwt body_string = Http_message.string_of_body body in
  lwt res = M.handler body_string in
  Http_daemon.respond ~body:res ()

let callback _ req =
  match Http_request.path req with
    | "/" -> Http_daemon.respond_file ~fname:"index.html" ~mime_type:"text/html" ()
    | "/_build/clicks.js" -> Http_daemon.respond_file ~fname:"_build/clicks.js" ~mime_type:"application/javascript" ()
    | "/clicks" -> clicks req
    | url -> Http_daemon.respond_error ~status:`Not_found ~body:("not found: " ^ url) ()

let exn_handler exn = Lwt.return ()

let spec = {
  Http_daemon.address = "0.0.0.0";
  auth = `None;
  callback = callback;
  conn_closed = ignore;
  port = 9007;
  root_dir = None;
  exn_handler = exn_handler;
  timeout = Some 15;
  auto_close = true;
}

let _ = Lwt_main.run (Http_daemon.main spec)
