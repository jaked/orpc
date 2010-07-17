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

  let modules =
    match mode with
      | Simple -> failwith "simple mode not supported for js_srv"
      | Modules kinds ->
          List.map
            (fun kind ->
               let mt, monad =
                 match kind with
                   | Ik_abstract -> assert false
                   | Sync -> "Sync", <:ident< Orpc_js_server.Sync >>
                   | Async -> "Async", <:ident< Orpc_js_server.Async >>
                   | Lwt -> "Lwt", <:ident< Lwt >> in
               <:sig_item<
                 module $uid:mt$ : functor (A : $uid:name$.$uid:mt$) ->
                 sig
                   val handler : string -> string $id:monad$.t
                 end
               >>)
            kinds in

  <:sig_item< $list:modules$ >>



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
               let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
               $G.args_apps <:expr< A.$lid:id$ >> args$
             >>)$)
    >> in

  let async_func (_, id, args, _) =
    <:expr<
      ($`str:id$,
       fun x0 rf ->
         $(fun body ->
             if has_excs
             then <:expr< pack_orpc_result_async (fun k -> $body$ k) >>
             else <:expr< $body$ >>)
           (let (ps, _) = G.vars args in
            <:expr<
              let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
              $G.args_apps <:expr< A.$lid:id$ >> args$
            >>)$
         (fun r -> let r = Obj.repr (r ()) in rf (fun () -> r)))
    >> in

  let lwt_func (_, id, args, _) =
    <:expr<
      ($`str:id$,
      fun x0 ->
        Lwt.try_bind
          (fun () ->
             $let (ps, _) = G.vars args in
             <:expr<
               let ( $tup:paCom_of_list ps$ ) = $id:to_arg id$ x0 in
               $G.args_apps <:expr< A.$lid:id$ >> args$
             >>$)
          (fun v ->
             Lwt.return
               (Obj.repr
                  ($if has_excs
                   then <:expr< Orpc.Orpc_success v >>
                   else <:expr< v >>$)))
          (fun e ->
             $if has_excs
             then <:expr< Lwt.return (Obj.repr (map_exns e)) >>
             else <:expr< Lwt.fail e >>$))
    >> in

  let modules =
    match mode with
      | Simple -> failwith "simple mode not supported for js_srv"
      | Modules kinds ->
          List.map
            (fun kind ->
               let mt, monad, func =
                 match kind with
                   | Ik_abstract -> assert false
                   | Sync -> "Sync", <:ident< Orpc_js_server.Sync >>, sync_func
                   | Async -> "Async", <:ident< Orpc_js_server.Async >>, async_func
                   | Lwt -> "Lwt", <:ident< Lwt >>, lwt_func in
               <:str_item<
                 module $uid:mt$ (A : $uid:name$.$uid:mt$) =
                 struct
                   module H = Orpc_js_server.Handler($id:monad$)
                   let handler = H.handler $G.conses (List.map func funcs)$
                 end
               >>)
            kinds in

  let pack_orpc_result () =
    let mc (_,id,ts) =
      match ts with
        | [] -> <:match_case< $id:qual_id id$ -> Orpc.Orpc_failure e >>
        | _ -> <:match_case< $id:qual_id id$ _ -> Orpc.Orpc_failure e >> in
    <:str_item<
      let map_exns e =
        match e with
          | $list:List.map mc excs$
          | _ -> raise e

      let pack_orpc_result f =
        try Orpc.Orpc_success (f ())
        with e -> map_exns e

      let pack_orpc_result_async f k =
        try f (fun r -> let r = try Orpc.Orpc_success (r ()) with e -> map_exns e in k (fun () -> r))
        with e -> let r = map_exns e in k (fun () -> r)
    >> in

  <:str_item<
    $if has_excs then pack_orpc_result () else <:str_item< >>$ ;;

    $list:modules$
  >>
