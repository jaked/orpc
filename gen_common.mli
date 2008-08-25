open Camlp4.PreCast

val arg : string -> string
val argi : string -> int -> string
val res : string -> string
val res0 : string -> string
val xdr : string -> string
val xdr_p : string -> string
val xdr_arg : string -> string
val xdr_res : string -> string
val to_ : string -> string
val to_p : string -> string
val to_arg : string -> string
val to_res : string -> string
val of_ : string -> string
val of_p : string -> string
val of_arg : string -> string
val of_res : string -> string
val aux_id : string -> string -> Ast.ident
val string_of_kind : S_ast.interface_kind -> string
val vars : 'a list -> Ast.patt list * Ast.expr list
val arrows : Ast.ctyp list -> Ast.ctyp -> Ast.ctyp
val tapps : Ast.ctyp -> Ast.ctyp list -> Ast.ctyp
val funs : Ast.patt list -> Ast.expr -> Ast.expr
val funs_ids : string list -> Ast.expr -> Ast.expr
val apps : Ast.expr -> Ast.expr list -> Ast.expr
val conses : Ast.expr list -> Ast.expr
val qual_id : string -> S_ast.mode -> string -> Ast.ident
val qual_id_aux : string -> S_ast.mode -> string -> Ast.ident
val gen_type : (S_ast.ident -> Ast.ident) -> S_ast.typ -> Ast.ctyp
val args_funs : S_ast.argtyp list -> Ast.expr -> Ast.expr
val args_apps : Ast.expr -> S_ast.argtyp list -> Ast.expr
val args_arrows : (S_ast.ident -> Ast.ident) -> S_ast.argtyp list -> Ast.ctyp -> Ast.ctyp
