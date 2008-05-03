open Camlp4.PreCast

exception Error of Loc.t * string

let loc_error loc err = raise (Error (loc, err))
let ctyp_error t err = loc_error (Ast.loc_of_ctyp t) err
let sig_item_error i err = loc_error (Ast.loc_of_sig_item i) err
