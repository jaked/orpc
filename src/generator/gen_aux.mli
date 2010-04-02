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

val gen_mli : string -> Types.interface -> Ast.sig_item
val gen_ml : string -> Types.interface -> Ast.str_item

val gen_sig_typedef : ?qual_id:(Types.ident -> Ast.ident) -> Types.typedef list -> Ast.sig_item
val gen_str_typedef : ?qual_id:(Types.ident -> Ast.ident) -> bool -> Types.typedef list -> Ast.str_item
