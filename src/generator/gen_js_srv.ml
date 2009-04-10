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
open Ast
open Types
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let gen_mli name (typedefs, excs, funcs, mode) =

  let qual_id = G.qual_id_aux name mode in

  let modules =
    match mode with
      | Simple -> []
      | Modules kinds ->
          List.map
            (fun kind ->
              if kind <> Sync then <:sig_item< >>
              else
                <:sig_item<
                  module Sync : functor (A : $uid:name$.Sync) ->
                  sig
                    val handler : string -> string
                  end
                >>)
            kinds in

  <:sig_item<
    val handler :
      $List.fold_right
        (fun (_, id, args, res) t ->
          <:ctyp<
            $lid:"proc_" ^ id$ :
              $G.args_arrows qual_id args (G.gen_type qual_id res)$
            -> $t$
          >>)
        funcs
        <:ctyp< string -> string >>$ ;;

    $list:modules$
  >>



let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id_aux name mode in

  let aux_id id = <:ident< $uid:name ^ "_js_aux"$ . $lid:id$ >> in
  let to_arg id = aux_id ("to_" ^ id ^ "'arg") in

  let sync_func (_, id, args, _) =
    <:expr<
      ($`str:id$,
      fun x0 ->
        Obj.repr
          $(fun body ->
              if has_excs
              then <:expr< pack_orpc_result (fun () -> $body$) >>
              else body)
            (let (ps, _) = G.vars args in
             <:expr<
               let ( $paCom_of_list ps$ ) = $id:to_arg id$ x0 in
               $G.args_apps <:expr< $lid:"proc_" ^ id$ >> args$
             >>)$)
    >> in

  let modules =
    match mode with
      | Simple -> []
      | Modules kinds ->
          List.map
            (fun kind ->
              if kind <> Sync then <:str_item< >>
              else
                <:str_item<
                  module Sync (A : $uid:name$.Sync) =
                  struct
                    let handler =
                      $List.fold_left
                        (fun e (_, id, args, _) ->
                          let body = <:expr< A.$lid:id$ >> in
                          ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, body)))
                        <:expr< handler >>
                        funcs$
                  end
                >>)
            kinds in

  let pack_orpc_result () =
    let mc (_,id,ts) =
      match ts with
        | [] -> <:match_case< $id:qual_id id$ -> Orpc.Orpc_failure e >>
        | [_] -> <:match_case< $id:qual_id id$ _ -> Orpc.Orpc_failure e >>
        | _ ->
            <:match_case<
              $id:qual_id id$ ($paCom_of_list (List.map (fun _ -> <:patt< _ >>) ts)$) -> Orpc.Orpc_failure e
            >> in
    <:str_item<
      let pack_orpc_result f =
        try Orpc.Orpc_success (f ())
        with e ->
          match e with
            | $list:List.map mc excs$
            | _ -> raise e
    >> in

  <:str_item<
    $if has_excs then pack_orpc_result () else <:str_item< >>$ ;;

    let handler =
      $List.fold_right
        (fun (_, id, _, _) e -> <:expr< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
        funcs
        <:expr< Orpc_js_server.handler $G.conses (List.map sync_func funcs)$ >>$ ;;

    $list:modules$
  >>
