class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit
end

class type document =
object
  method getElementById : string -> < ..>
end

class type span =
object
  method _set_innerHTML : string -> unit
end

class type button =
object
  method _set_onclick : (unit -> unit) Ocamljs.jsfun -> unit
end

(* FireBug console *)
class type console =
object
  method log : string -> unit
end

let window = (Ocamljs.var "window" : window)
let document = (Ocamljs.var "document" : document)
let console = (Ocamljs.var "console" : console)

(* module Server = Proto_clnt(struct val url = "/clicks" end) *)
module Server =
struct
  let n = ref 0

  let clicks () = Lwt.return (!n)
  let click () = incr n; Lwt.return (!n)
end

let (>>=) = Lwt.(>>=)

;;

window#_set_onload (Ocamljs.jsfun (fun () ->
  let clicks = (document#getElementById "clicks" : span) in
  let click = (document#getElementById "click" : button) in

  let set_clicks n = Lwt.return (clicks#_set_innerHTML (string_of_int n)) in

  click#_set_onclick (Ocamljs.jsfun (fun () ->
    ignore(Server.click () >>= set_clicks)));

  ignore(Server.clicks () >>= set_clicks)))
