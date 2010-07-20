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
          module $uid:mt$(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) : $uid:name$.$uid:mt$
        >>)
      kinds in

  <:sig_item<
    val create_client :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      Rpc_client.connector ->
      Rpc.protocol ->
      Rpc_client.t

    val create_portmapped_client :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      string ->
      Rpc.protocol ->
      Rpc_client.t

    val create_client2 :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      Rpc_client.mode2 ->
      Rpc_client.t ;;

    $list:modules$
  >>

let gen_ml name (typedefs, excs, funcs, kinds) =

  let has_excs = excs <> [] in
  let of_arg = G.of_arg name in
  let to_res = G.to_res name in

  let modules =
    List.map
      (fun kind ->
        let func (_, id, args, res) =
          let body =
            match kind with
              | Ik_abstract -> assert false

              | Sync ->
                  let body2 = <:expr< $id:to_res id$ (Rpc_client.sync_call c $`str:id$ ($id:of_arg id$ x0)) >> in
                  if has_excs
                  then <:expr< C.with_client (fun c -> Orpc.unpack_orpc_result $body2$) >>
                  else <:expr< C.with_client (fun c -> $body2$) >>

              | Lwt ->
                  <:expr<
                    let t, u = Lwt.wait () in
                    C.with_client (fun c ->
                      Rpc_client.add_call c $`str:id$ ($id:of_arg id$ x0)
                        (fun g ->
                           $if has_excs
                            then <:expr< try Lwt.wakeup u (Orpc.unpack_orpc_result ($id:to_res id$ (g ()))) with e -> Lwt.wakeup_exn u e >>
                            else <:expr< Lwt.wakeup u ($id:to_res id$ (g ())) >>$));
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
          module $uid:G.string_of_kind kind$(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) =
          struct
            $G._r_of_kind kind$;;
            $list:List.map func funcs$
          end
        >>)

      kinds in

  <:str_item<
    let create_client
        ?(esys = Unixqueue.create_unix_event_system())
        ?program_number
        ?version_number
        connector
        protocol =
      Rpc_client.create ?program_number ?version_number esys connector protocol $id:G.program name$

    let create_portmapped_client ?esys ?program_number ?version_number host protocol =
      create_client ?esys ?program_number ?version_number (Rpc_client.Portmapped host) protocol

    let create_client2
        ?(esys = Unixqueue.create_unix_event_system())
        ?program_number
        ?version_number
        mode2 =
      Rpc_client.create2 ?program_number ?version_number mode2 $id:G.program name$ esys ;;

    $list:modules$
  >>
