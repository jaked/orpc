open Camlp4.PreCast

val to_ : string -> string
val to_p : string -> string
val to_arg : string -> string -> Ast.ident
val to_res : string -> string -> Ast.ident
val of_ : string -> string
val of_p : string -> string
val of_arg : string -> string -> Ast.ident
val of_res : string -> string -> Ast.ident
val program : string -> Ast.ident
val string_of_kind : Types.interface_kind -> string
val vars : 'a list -> Ast.patt list * Ast.expr list
val tvars : string list -> Ast.ctyp list
val arrows : Ast.ctyp list -> Ast.ctyp -> Ast.ctyp
val tapps : Ast.ctyp -> Ast.ctyp list -> Ast.ctyp
val funs : Ast.patt list -> Ast.expr -> Ast.expr
val funs_ids : string list -> Ast.expr -> Ast.expr
val apps : Ast.expr -> Ast.expr list -> Ast.expr
val conses : Ast.expr list -> Ast.expr
val qual_id : string -> Types.mode -> string -> Ast.ident
val qual_id_aux : string -> Types.mode -> string -> Ast.ident
val gen_type : (Types.ident -> Ast.ident) -> Types.typ -> Ast.ctyp
val args_funs : Types.argtyp list -> Ast.expr -> Ast.expr
val args_apps : Ast.expr -> Types.argtyp list -> Ast.expr
val args_arrows : (Types.ident -> Ast.ident) -> Types.argtyp list -> Ast.ctyp -> Ast.ctyp
