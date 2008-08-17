open Camlp4.PreCast
open Ast
open S_ast
open Error

let _ = let module M = Camlp4OCamlRevisedParser.Make(Syntax) in ()
let _ = let module M = Camlp4OCamlParser.Make(Syntax) in ()

module Loc = Camlp4.PreCast.Loc

let do_file fn =
  let print_error loc msg =
    Format.fprintf Format.std_formatter
      "%s at %a\n" msg Loc.print loc;
    Format.print_flush () in
  try
    let ch = open_in fn in
    let st = Stream.of_channel ch in
    let i = Syntax.parse_interf (Loc.mk fn) st in
    let intf = Parse.parse_interface i in
    let intf = Check.check_interface intf in

    let base = Filename.chop_extension fn in
    let mod_base = String.capitalize (Filename.basename base) in
    List.iter
      (fun (ext, gen_mli, gen_ml) ->
        Printers.OCaml.print_interf ~output_file:(base ^ "_" ^ ext ^ ".mli") (gen_mli mod_base intf);
        Printers.OCaml.print_implem ~output_file:(base ^ "_" ^ ext ^ ".ml") (gen_ml mod_base intf))
      [
        "aux", Gen_aux.gen_aux_mli, Gen_aux.gen_aux_ml;
        "clnt", Gen_clnt.gen_clnt_mli, Gen_clnt.gen_clnt_ml;
        "srv", Gen_srv.gen_srv_mli, Gen_srv.gen_srv_ml;
        "hook", Gen_hook.gen_hook_mli, Gen_hook.gen_hook_ml;
        "trace", Gen_trace.gen_trace_mli, Gen_trace.gen_trace_ml;
      ]
  with
    | Loc.Exc_located (loc, Stream.Error msg) -> print_error loc msg
    | Loc.Exc_located (loc, e) -> print_error loc (Printexc.to_string e)
    | Error (loc, msg) -> print_error loc msg

let args = Arg.align [
]

let _ = Arg.parse args do_file "usage:"
