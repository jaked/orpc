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
            val bind : Orpc_js_client.t -> unit
          end
        >>)
      kinds in

  <:sig_item< $list:modules$ >>

let gen_ml name (typedefs, excs, funcs, kinds) =

  let has_excs = excs <> [] in

  let modules =
    List.map
      (fun kind ->
         let mt = G.string_of_kind kind in
         let func (_, id, args, res) =
           let body =
             let (ps, _) = G.vars args in
             <:expr<
               let ( $tup:paCom_of_list ps$ ) = Obj.obj x0 in
               $G.args_apps <:expr< A.$lid:id$ >> args$
             >> in
           match kind with
             | Ik_abstract -> assert false

             | Sync ->
                 if has_excs
                 then
                   <:expr<
                     ($`str:id$,
                      fun x0 ->
                        Lwt.return (Obj.magic (Orpc.pack_orpc_result (fun () -> $body$))))
                   >>
                 else
                   <:expr<
                     ($`str:id$,
                      fun x0 ->
                        try Lwt.return (Obj.repr $body$)
                        with e -> Lwt.fail e)
                   >>

             | Lwt ->
                 if has_excs
                 then
                   <:expr<
                     ($`str:id$,
                      fun x0 ->
                        Obj.magic
                          (Lwt.try_bind
                             (fun () -> $body$)
                             (fun v -> Lwt.return (Orpc.Orpc_success (Obj.repr v)))
                             (fun e -> Lwt.return (Orpc.Orpc_failure e)))) (* XXX check for declared exception types *)
                   >>
                 else
                   <:expr<
                     ($`str:id$,
                      fun x0 -> Obj.magic $body$)
                   >> in

        <:str_item<
          module $uid:mt$(A : $uid:name$.$uid:mt$) =
          struct
            let bind t = Orpc_js_client.bind t $G.conses (List.map func funcs)$
          end
        >>)
      kinds in

  <:str_item< $list:modules$ >>
