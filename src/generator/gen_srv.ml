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
                module $uid:mt$ : functor (A : $uid:name$.$uid:mt$) ->
                sig
                  val bind :
                    ?program_number:Rtypes.uint4 ->
                    ?version_number:Rtypes.uint4 ->
                    Rpc_server.t -> unit
                end
              >>)
            kinds in

  <:sig_item<
    val bind :
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      $List.fold_right
        (fun (_, id, args, res) t ->
          <:ctyp<
            $lid:"proc_" ^ id$ :
              $G.args_arrows qual_id args (G.gen_type qual_id res)$
            -> $t$
          >>)
        funcs
        <:ctyp< Rpc_server.t -> unit >>$ ;;

    val bind_async :
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      $List.fold_right
        (fun (_, id, args, res) t ->
          <:ctyp<
            $lid:"proc_" ^ id$ : (Rpc_server.session ->
              $G.args_arrows qual_id args
                <:ctyp< ($G.gen_type qual_id res$ -> unit) -> unit >>$)
            -> $t$
          >>)
        funcs
        <:ctyp< Rpc_server.t -> unit >>$ ;;

    $list:modules$
  >>



let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let to_arg = G.to_arg name in
  let of_res = G.of_res name in

  let sync_func (_, id, args, _) =
    <:expr<
      Rpc_server.Sync {
        Rpc_server.sync_name = $`str:id$;
        Rpc_server.sync_proc = fun x0 ->
          $id:of_res id$
          $(fun body ->
              if has_excs
              then <:expr< Orpc.pack_orpc_result (fun () -> $body$) >>
              else body)
            (let (ps, _) = G.vars args in
             <:expr<
               let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
               $G.args_apps <:expr< $lid:"proc_" ^ id$ >> args$
             >>)$
      }
    >> in

  let async_func (_, id, args, _) =
    <:expr<
      Rpc_server.Async {
        Rpc_server.async_name = $`str:id$;
        Rpc_server.async_invoke = fun s x0 ->
          Orpc_onc.session := Some s;
          $(fun body body2 ->
              if has_excs
              then <:expr< Orpc.pack_orpc_result_async (fun k -> $body$ k) $body2$ >>
              else <:expr< $body$ $body2$ >>)
            (let (ps, _) = G.vars args in
             <:expr<
               let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
               $G.args_apps <:expr< $lid:"proc_" ^ id$ s >> args$
             >>)
            <:expr< (fun y -> try Rpc_server.reply s ($id:of_res id$ y) with _ -> ()) >>$
        }
      >> in

  let lwt_func (_, id, args, _) =
    <:expr<
      Rpc_server.Async {
        Rpc_server.async_name = $`str:id$;
        Rpc_server.async_invoke = fun s x0 ->
          Orpc_onc.session := Some s;
          Lwt.ignore_result
            (Lwt.try_bind
                (fun () ->
                  $let (ps, _) = G.vars args in
                   <:expr<
                     let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
                     $G.args_apps <:expr< A.$lid:id$ >> args$
                   >>$)
                (fun v ->
                  (try
                    Rpc_server.reply s
                      ($id:of_res id$
                          $if has_excs
                           then <:expr< Orpc.Orpc_success v >>
                           else <:expr< v >>$);
                  with _ -> ());
                  Lwt.return ())
                (fun e ->
                  $if has_excs
                   then
                     <:expr<
                       (try Rpc_server.reply s ($id:of_res id$ (Orpc.Orpc_failure e)) with _ -> ());
                       Lwt.return ()
                     >>
                   else <:expr< raise e >>$))
        }
      >> in

  let modules =
    match mode with
      | Simple -> []
      | Modules kinds ->
          List.map
            (fun kind ->
              let mt = G.string_of_kind kind in
              <:str_item<
                module $uid:mt$ (A : $uid:name$.$uid:mt$) =
                struct
                  let bind
                      ?program_number
                      ?version_number
                      srv =
                    $match kind with
                      | Ik_abstract -> assert false

                      | Sync ->
                          List.fold_left
                            (fun e (_, id, args, _) ->
                              let body = <:expr< A.$lid:id$ >> in
                              ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, body)))
                            <:expr< bind ?program_number ?version_number >>
                            funcs
                      | Async ->
                          List.fold_left
                            (fun e (_, id, args, _) ->
                              let body =
                                <:expr<
                                  fun s ->
                                    $G.args_funs args
                                      <:expr<
                                        fun pass_reply ->
                                          Orpc_onc.session := Some s;
                                          $G.args_apps <:expr< A.$lid:id$ >> args$
                                            (fun r -> pass_reply (r ()))
                                      >>$
                                >> in
                              ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, body)))
                            <:expr< bind_async ?program_number ?version_number >>
                            funcs
                      | Lwt ->
                          <:expr<
                            Rpc_server.bind
                              ?program_number ?version_number $id:G.program name$
                              $G.conses (List.map lwt_func funcs)$
                          >>$
                    srv
                end
              >>)
            kinds in

  <:str_item<
    let bind
        ?program_number
        ?version_number =
      $List.fold_right
        (fun (_, id, _, _) e -> <:expr< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
        funcs
        <:expr<
          fun srv ->
            Rpc_server.bind
              ?program_number ?version_number $id:G.program name$
              $G.conses (List.map sync_func funcs)$
              srv
        >>$ ;;

    let bind_async
        ?program_number
        ?version_number =
      $List.fold_right
        (fun (_, id, _, _) e -> <:expr< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
        funcs
        <:expr<
          fun srv ->
            Rpc_server.bind
              ?program_number ?version_number $id:G.program name$
              $G.conses (List.map async_func funcs)$
              srv
        >>$ ;;

    $list:modules$
  >>
