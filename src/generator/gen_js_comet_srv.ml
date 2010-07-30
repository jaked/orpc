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
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let gen_mli name (typedefs, excs, funcs, kinds) =

  let modules =
    List.map
      (function
         | Ik_abstract -> assert false
         | Sync -> <:sig_item< >>
         | Lwt ->
             <:sig_item<
               module Lwt(S : sig val server : Orpc_js_comet_server.t val session : Orpc_js_comet_server.session end) : $uid:name$.Lwt
             >>)
      kinds in

  <:sig_item< $list:modules$ >>

let gen_ml name (typedefs, excs, funcs, kinds) =

  let has_excs = excs <> [] in

  let aux_id id = <:ident< $uid:name ^ "_js_aux"$ . $lid:id$ >> in
  let of_arg id = aux_id ("of_" ^ id ^ "'arg") in
  let to_res id = aux_id ("to_" ^ id ^ "'res") in

  let modules =
    List.map
      (fun kind ->
         match kind with
           | Ik_abstract -> assert false
           | Sync -> <:str_item< >>
           | Lwt ->
               let func (_, id, args, res) =
                 let body2 = <:expr< Orpc_js_comet_server.call S.server S.session $`str:id$ ($id:of_arg id$ x0) >> in
                 let body =
                   if has_excs
                   then <:expr< Lwt.bind $body2$ (fun v -> Lwt.return (Orpc.unpack_orpc_result ($id:to_res id$ v))) >>
                   else <:expr< Lwt.bind $body2$ (fun v -> Lwt.return ($id:to_res id$ v)) >> in
                 <:str_item<
                   let $lid:id$ =
                     $G.args_funs args
                       (match args with
                          | [] -> assert false
                          | [a] -> body
                          | _ ->
                              let (_, es) = G.vars args in
                              <:expr< let x0 = ($exCom_of_list es$) in $body$ >>)$
                 >> in

               <:str_item<
                 module Lwt(S : sig val server : Orpc_js_comet_server.t val session : Orpc_js_comet_server.session end) =
                 struct
                   $G._r_of_kind kind$;;
                   $list:List.map func funcs$
                 end
               >>)
      kinds in

  <:str_item< $list:modules$ >>
