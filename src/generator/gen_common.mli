(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
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
