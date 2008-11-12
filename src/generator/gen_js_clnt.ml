(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008 Skydeck, Inc
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
              let mt = G.string_of_kind kind in
              <:sig_item<
                module $uid:mt$(C : sig val with_client : (Orpc_js_client.t -> 'a) -> 'a end) : $uid:name$.$uid:mt$
              >>)
            kinds in

  <:sig_item<
    $sgSem_of_list
      (List.map
          (fun (_, id, args, res) ->
            <:sig_item<
              val $lid:id$ : Orpc_js_client.t ->
                $G.args_arrows qual_id args (G.gen_type qual_id res)$
            >>)
          funcs)$ ;;

    $sgSem_of_list
      (List.map
          (fun (_, id, args, res) ->
            <:sig_item<
              val $lid:id ^ "'async"$ : Orpc_js_client.t ->
                $G.args_arrows qual_id args
                  <:ctyp< ((unit -> $G.gen_type qual_id res$) -> unit) -> unit >>$
            >>)
          funcs)$ ;;

    $sgSem_of_list modules$
  >>

let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in

  let sync_func (_, id, args, res) =
    (fun body ->
      <:str_item<
        let $lid:id$ = fun client ->
          $match args with
            | [] -> assert false
            | [a] -> G.args_funs args body
            | _ ->
                let (_, es) = G.vars args in
                G.args_funs args
                  <:expr< let x0 = ($exCom_of_list es$) in $body$ >>$
      >>)
      ((fun body2 ->
          if has_excs
          then <:expr< Orpc.unpack_orpc_result $body2$ >>
          else body2)
        <:expr< Obj.obj (Orpc_js_client.sync_call client $`str:id$ (Obj.repr x0)) >>) in

  let async_func (_, id, args, res) =
    (fun body ->
      <:str_item<
        let $lid:id ^ "'async"$ = fun client ->
          $match args with
            | [] -> assert false
            | [a] -> G.args_funs args <:expr< fun pass_reply -> $body$ >>
            | _ ->
                let (_, es) = G.vars args in
                G.args_funs args
                  <:expr< fun pass_reply -> let x0 = ($exCom_of_list es$) in $body$ >>$
      >>)
      <:expr<
        Orpc_js_client.add_call client $`str:id$ (Obj.repr x0)
          (fun g -> pass_reply (fun () ->
            $if has_excs
            then <:expr< Orpc.unpack_orpc_result (Obj.obj (g ())) >>
            else <:expr< Obj.obj (g ()) >>$))
      >> in

  let modules =
    match mode with
      | Simple -> []
      | Modules kinds ->
          List.map
            (fun kind ->
              let func (_, id, args, res) =
                <:str_item<
                  let $lid:id$ =
                    $G.args_funs args
                      (match kind with
                        | Sync ->
                            <:expr<
                              C.with_client
                                (fun c -> $G.args_apps <:expr< $lid:id$ c >> args$)
                            >>
                        | Async ->
                            <:expr<
                              fun pass_reply ->
                                C.with_client
                                  (fun c ->
                                    $G.args_apps <:expr< $lid:id ^ "'async"$ c >> args$
                                      pass_reply)
                            >>
                        | Lwt ->
                            <:expr<
                              C.with_client
                                (fun c ->
                                  let res = Lwt.wait () in
                                  $G.args_apps <:expr< $lid:id ^ "'async"$ c >> args$
                                    (fun r ->
                                      try Lwt.wakeup res (r ())
                                      with exn -> Lwt.wakeup_exn res exn);
                                  res)
                            >>)$
                >> in

              <:str_item<
                module $uid:G.string_of_kind kind$(C : sig val with_client : (Orpc_js_client.t -> 'a) -> 'a end) =
                struct
                  $stSem_of_list (List.map func funcs)$
                end
              >>)

            kinds in

  <:str_item<
    $stSem_of_list (List.map sync_func funcs)$ ;;

    $stSem_of_list (List.map async_func funcs)$ ;;

    $stSem_of_list modules$
  >>
