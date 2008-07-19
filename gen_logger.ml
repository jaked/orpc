open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let g = Camlp4.PreCast.Loc.ghost

let gen_logger_ml name (typedefs, excs, funcs, kinds) =
  let func (_, id, args, res) =
    let (ps, es) = G.vars args in
        <:str_item@g<
          let $lid:id$ =
            $G.funs
              (List.map2 G.labelled_patt args ps)
              <:expr@g<
                let now = Unix.gettimeofday() in
                try
                  let r = $G.apps <:expr@g< A.$lid:id$>>
                    (List.map2 G.labelled_expr args es)$
                  in
                  L.timing $`str:id$ (Unix.gettimeofday() -. now);
                  r
                with e ->
                  L.timing $`str:id$ ~exn:e (Unix.gettimeofday() -. now);
                  raise e
              >>$
        >>
  in


  <:str_item@g<
    module Sync (A : $uid:name$.Sync)(L : sig val timing : string -> ?exn:exn -> float -> unit end) =
    struct
       $stSem_of_list (List.map func funcs)$
    end
  >>
