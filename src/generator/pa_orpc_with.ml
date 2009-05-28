open Camlp4.PreCast
open Orpc_generator

let ocamljs = ref false

let _ = Camlp4.Options.add "--ocamljs" (Arg.Unit (fun b -> ocamljs := true)) "stub out server-side code"

let ds_of_ctyp ctyp = Parse.parse_typedef (Ast.loc_of_ctyp ctyp) ctyp

let add_gens tag no_ocamljs gen_sig gen_str =
  Pa_type_conv.add_sig_generator tag (fun ctyp -> gen_sig (ds_of_ctyp ctyp));
  Pa_type_conv.add_generator tag (fun ctyp -> gen_str (no_ocamljs && !ocamljs) (ds_of_ctyp ctyp))

let _ =
  add_gens "orpc_aux" true Gen_aux.gen_sig_typedef Gen_aux.gen_str_typedef;
  add_gens "orpc_js_aux" true Gen_js_aux.gen_sig_typedef Gen_js_aux.gen_str_typedef;
  add_gens "orpc_trace" false Gen_trace.gen_sig_typedef (Gen_trace.gen_str_typedef ~rec_mod:false)
