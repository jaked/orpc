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

  <:sig_item< $list:modules$ >>



let gen_ml name (typedefs, excs, funcs, kinds) =

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
               $G.args_apps <:expr< A.$lid:id$ >> args$
             >>)$
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
                    <:expr<
                      Rpc_server.bind
                        ?program_number ?version_number $id:G.program name$
                        $G.conses (List.map sync_func funcs)$
                    >>
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

  <:str_item< $list:modules$ >>
