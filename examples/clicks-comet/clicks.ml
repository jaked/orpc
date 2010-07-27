let (>>=) = Lwt.(>>=)

;;

Dom.window#_set_onload begin fun () ->
  let client = Orpc_js_client.create ~transport:`Xhr_long_poll "/clicks" in

  let clicks = (Dom.document#getElementById "clicks" : Dom.span) in
  let set_clicks n = clicks#_set_innerHTML (string_of_int n) in

  let module M = Comet_js_comet_clnt.Sync(struct let set_clicks = set_clicks end) in
  M.bind client;

  let module Server = Proto_js_clnt.Lwt(struct let with_client f = f client end) in

  ignore
    (Orpc_js_client.connect client >>=
       Server.clicks >>=
         fun n -> set_clicks n; Lwt.return ());

  let click = (Dom.document#getElementById "click" : Dom.button) in
  click#_set_onclick (fun _ ->
    ignore(Server.click ());
    false)
end
