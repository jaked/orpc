open Camlp4.PreCast
open Orpc_generator

type param = Aux | Js_aux | Trace

let orpc_params = Gram.Entry.mk "orpc_params"

EXTEND Gram

GLOBAL: orpc_params;

  orpc_param: [[ 
                 "aux" -> Aux
               | "js_aux" -> Js_aux
               | "trace" -> Trace
               ]];

  orpc_params: [[
                  l = LIST1 orpc_param SEP "," -> l
                ]];
END

let ocamljs = ref false

let _ = Camlp4.Options.add "--ocamljs" (Arg.Unit (fun b -> ocamljs := true)) "stub out server-side code"

let ds_of_ctyp ?allow_abstract ctyp = Parse.parse_typedef ?allow_abstract (Ast.loc_of_ctyp ctyp) ctyp

;;

Pa_type_conv.add_sig_generator_with_arg "orpc" orpc_params
  (fun ctyp args ->
     let args = match args with None -> [ Aux; Js_aux; Trace ] | Some args -> args in (* XXX raise error instead? *)
     let ds = ds_of_ctyp ~allow_abstract:true ctyp in
     let _loc = Ast.loc_of_ctyp ctyp in
     <:sig_item< $list:
       List.map
         (function
            | Aux -> Gen_aux.gen_sig_typedef ds
            | Js_aux -> Gen_js_aux.gen_sig_typedef ds
            | Trace -> Gen_trace.gen_sig_typedef ds)
         args$
     >>);

Pa_type_conv.add_generator_with_arg "orpc" orpc_params
  (fun ctyp args ->
     let args = match args with None -> [ Aux; Js_aux; Trace ] | Some args -> args in (* XXX raise error instead? *)
     let ds = ds_of_ctyp ctyp in
     let _loc = Ast.loc_of_ctyp ctyp in
     <:str_item< $list:
       List.map
         (function
            | Aux -> Gen_aux.gen_str_typedef !ocamljs ds
            | Js_aux -> Gen_js_aux.gen_str_typedef !ocamljs ds
            | Trace -> Gen_trace.gen_str_typedef false ~rec_mod:false ds)
         args$
     >>);
