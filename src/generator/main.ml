(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Copyright (C) 2010 Jacob Donham
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA
 *)

open Camlp4.PreCast
open Ast
open Types
open Error

let _ = let module M = Camlp4OCamlRevisedParser.Make(Syntax) in ()
let _ = let module M = Camlp4OCamlParser.Make(Syntax) in ()

module Loc = Camlp4.PreCast.Loc

let modules = [
  ref false, "aux", Gen_aux.gen_mli, Gen_aux.gen_ml;
  ref false, "clnt", Gen_clnt.gen_mli, Gen_clnt.gen_ml;
  ref false, "srv", Gen_srv.gen_mli, Gen_srv.gen_ml;
  ref false, "trace", Gen_trace.gen_mli, Gen_trace.gen_ml;
  ref false, "js_aux", Gen_js_aux.gen_mli, Gen_js_aux.gen_ml;
  ref false, "js_clnt", Gen_js_clnt.gen_mli, Gen_js_clnt.gen_ml;
  ref false, "js_srv", Gen_js_srv.gen_mli, Gen_js_srv.gen_ml;
  ref false, "js_comet_clnt", Gen_js_comet_clnt.gen_mli, Gen_js_comet_clnt.gen_ml;
  ref false, "js_comet_srv", Gen_js_comet_srv.gen_mli, Gen_js_comet_srv.gen_ml;
]

let do_file fn =
  let print_error loc msg =
    Format.fprintf Format.std_formatter
      "%s at %a\n" msg Loc.print loc;
    Format.print_flush ();
    exit 1 in
  try
    let ch = open_in fn in
    let st = Stream.of_channel ch in
    let i = Syntax.parse_interf (Loc.mk fn) st in
    let intf = Parse.parse_interface i in
    let intf = Check.check_interface intf in

    let base = Filename.chop_extension fn in
    let mod_base = String.capitalize (Filename.basename base) in

    let modules = List.filter (fun (flag, _, _, _) -> !flag) modules in

    ListLabels.iter modules ~f:(fun (_, ext, gen_mli, gen_ml) ->
      Printers.OCaml.print_interf ~output_file:(base ^ "_" ^ ext ^ ".mli") (gen_mli mod_base intf);
      Printers.OCaml.print_implem ~output_file:(base ^ "_" ^ ext ^ ".ml") (gen_ml mod_base intf))
  with
    | Loc.Exc_located (loc, Stream.Error msg) -> print_error loc msg
    | Loc.Exc_located (loc, e) -> print_error loc (Printexc.to_string e)
    | Error (loc, msg) -> print_error loc msg

let args =
  Arg.align
    (List.map
       (fun (flag, ext, _, _) ->
          "--" ^ ext, Arg.Set flag, "generate *_" ^ ext ^ ".ml[i]")
       modules)

let _ = Arg.parse args do_file "usage:"
