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
               module Lwt(C : sig val with_client : (Orpc_js_client.t -> 'a) -> 'a end) : $uid:name$.Lwt
             >>)
      kinds in

  <:sig_item< $list:modules$ >>

let gen_ml name (typedefs, excs, funcs, kinds) =

  let qual_id = G.qual_id_aux name in

  let has_excs = excs <> [] in

  let modules =
    List.map
      (fun kind ->
        let func (_, id, args, res) =
          match kind with
            | Ik_abstract -> assert false
            | Sync -> <:str_item< >>
            | Lwt ->
                let body =
                  <:expr<
                    let t, u = Lwt.wait () in
                    C.with_client (fun c ->
                      Orpc_js_client.call c $`str:id$ (Obj.repr x0)
                        (fun g ->
                          $if has_excs
                          then <:expr< try Lwt.wakeup u (unpack_orpc_result g) with e -> Lwt.wakeup_exn u e >>
                          else <:expr< Lwt.wakeup u (Obj.obj (g ())) >>$));
                    t
                  >> in

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
          module $uid:G.string_of_kind kind$(C : sig val with_client : (Orpc_js_client.t -> 'a) -> 'a end) =
          struct
            $G._r_of_kind kind$;;
            $list:List.map func funcs$
          end
        >>)
      kinds in

  (* exceptions are pointer-compared, so we need to map back to the right ones *)
  let unpack_orpc_result () =
    let string_of_ident = function
      | IdAcc (_, IdUid (_, m), IdUid (_, e)) -> m ^ "." ^ e
      | _ -> assert false in
    let mc (_,id,ts) =
      <:match_case<
          $`str:string_of_ident (qual_id id)$ ->
            $G.apps
              <:expr< $id:qual_id id$ >>
              (List.mapi (fun _ i -> <:expr< Obj.obj (Obj.field o $`int:i+1$) >>) ts)$
      >> in
    <:str_item<
      let unpack_orpc_result g =
        match Obj.obj (g ())
        with
          | Orpc.Orpc_success v -> v
          | Orpc.Orpc_failure e ->
              let o = Obj.repr e in
              let name = Obj.obj (Obj.field (Obj.field o 0) 0) in
              let e = match name with
                  $list:List.map mc excs$
                | _ -> e in
              raise e
    >> in

  <:str_item<
    $if has_excs then unpack_orpc_result () else <:str_item< >>$ ;;

    $list:modules$
  >>
