module Server =
  Proto_js_clnt.Lwt(struct let with_client f = f (Orpc_js_client.create "/clicks") end)

let (>>=) = Lwt.(>>=)

;;

Dom.window#_set_onload (Ocamljs.jsfun (fun () ->
  let clicks = (Dom.document#getElementById "clicks" : Dom.span) in
  let click = (Dom.document#getElementById "click" : Dom.button) in

  let set_clicks n = Lwt.return (clicks#_set_innerHTML (string_of_int n)) in

  click#_set_onclick (Ocamljs.jsfun (fun () ->
    ignore(Server.click () >>= set_clicks)));

  ignore(Server.clicks () >>= set_clicks)))
