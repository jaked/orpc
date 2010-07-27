open Cohttp
open Cohttpserver

let server = Orpc_js_comet_server.create ()

module Comet_js_comet_srv (S : sig val server : Orpc_js_comet_server.t val session : Orpc_js_comet_server.session end) =
struct
  let set_clicks n =
    lwt o = Orpc_js_comet_server.call S.server S.session "set_clicks" (Comet_js_aux.of_set_clicks'arg n) in
    try Lwt.return (Comet_js_aux.to_set_clicks'res o)
    with e -> Lwt.fail e
end

module Server =
struct
  let n = ref 0

  let clicks () = !n

  let click () =
    incr n;
    Orpc_js_comet_server.iter_sessions server
      (fun _ sess ->
         let module M = Comet_js_comet_srv (struct let server = server let session = sess end) in
         ignore (M.set_clicks !n))
end

let _ = Orpc_js_comet_server.bind server [
  "click", (fun x0 -> Lwt.return (Proto_js_aux.of_click'res (Server.click (Proto_js_aux.to_click'arg x0))));
  "clicks", (fun x0 -> Lwt.return (Proto_js_aux.of_clicks'res (Server.clicks (Proto_js_aux.to_clicks'arg x0))));
]

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
