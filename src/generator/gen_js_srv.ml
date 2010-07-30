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
      (fun kind ->
         let mt, ret =
           match kind with
             | Ik_abstract -> assert false
             | Sync -> "Sync", (fun t -> t)
             | Lwt -> "Lwt", (fun t -> TyApp (_loc, <:ctyp< Lwt.t >>, t)) in
         <:sig_item<
           module $uid:mt$ : functor (A : $uid:name$.$uid:mt$) ->
           sig
             val handler : string -> $ret <:ctyp< string >>$
             val funcs : (string * (Orpc_js_server.obj -> $ret <:ctyp< Orpc_js_server.obj >>$)) list
           end
         >>)
      kinds in

  <:sig_item< $list:modules$ >>



let gen_ml name (typedefs, excs, funcs, kinds) =

  let has_excs = excs <> [] in

  let aux_id id = <:ident< $uid:name ^ "_js_aux"$ . $lid:id$ >> in
  let to_arg id = aux_id ("to_" ^ id ^ "'arg") in
  let of_res id = aux_id ("of_" ^ id ^ "'res") in

  let modules =
    List.map
      (fun kind ->
        let func (_, id, args, _) =
          let body =
            match args with
              | [] -> assert false
              | [ _ ] -> <:expr< A.$lid:id$ ($id:to_arg id$ x0) >>
              | _ ->
                  let (ps, _) = G.vars args in
                   <:expr<
                     let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
                     $G.args_apps <:expr< A.$lid:id$ >> args$
                   >> in
      
          match kind with
            | Ik_abstract -> assert false
      
            | Sync ->
                <:expr<
                  ($`str:id$,
                  fun x0 ->
                    $id:of_res id$
                      $if has_excs
                       then <:expr< Orpc.pack_orpc_result (fun () -> $body$) >>
                       else body$)
                >>
      
            | Lwt ->
                if has_excs
                then
                  <:expr<
                    ($`str:id$,
                    fun x0 ->
                      Lwt.try_bind
                        (fun () -> $body$)
                        (fun v -> Lwt.return ($id:of_res id$ (Orpc.Orpc_success v)))
                        (fun e -> Lwt.return ($id:of_res id$ (Orpc.Orpc_failure e))))
                  >>
                else
                  <:expr<
                    ($`str:id$,
                    fun x0 ->
                      Lwt.bind $body$ (fun v -> Lwt.return ($id:of_res id$ v)))
                  >> in

         let mt, monad =
           match kind with
             | Ik_abstract -> assert false
             | Sync -> "Sync", <:ident< Orpc_js_server.Sync >>
             | Lwt -> "Lwt", <:ident< Lwt >> in
         <:str_item<
           module $uid:mt$ (A : $uid:name$.$uid:mt$) =
           struct
             let funcs = $G.conses (List.map func funcs)$
             let handler =
               let module H = Orpc_js_server.Handler($id:monad$) in
               H.handler funcs
           end
         >>)
      kinds in

  <:str_item< $list:modules$ >>
