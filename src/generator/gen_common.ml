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

let _loc = Camlp4.PreCast.Loc.ghost

let to_ id = "to_" ^ id
let to_p id = "to'" ^ id
let of_ id = "of_" ^ id
let of_p id = "of'" ^ id

let aux_id name id = <:ident< $uid:name ^ "_aux"$ . $lid:id$ >>
let to_arg name id = aux_id name ("to_" ^ id ^ "'arg")
let to_res name id = aux_id name ("to_" ^ id ^ "'res")
let of_arg name id = aux_id name ("of_" ^ id ^ "'arg")
let of_res name id = aux_id name ("of_" ^ id ^ "'res")
let program name = aux_id name "program"

let string_of_kind = function
  | Sync -> "Sync"
  | Async -> "Async"
  | Lwt -> "Lwt"

let vars l =
  let ps = List.mapi (fun _ i -> <:patt< $lid:"x" ^ string_of_int i$ >>) l in
  let es = List.mapi (fun _ i -> <:expr< $lid:"x" ^ string_of_int i$ >>) l in
  (ps, es)

let tvars vars =
  List.map (fun v -> <:ctyp< '$lid:v$ >>) vars

let arrows ts t =
  List.fold_right
    (fun t a -> <:ctyp< $t$ -> $a$ >>)
    ts
    t

let tapps t ts =
  List.fold_left
    (fun t t' -> <:ctyp< $t'$ $t$ >>)
    t
    ts

let funs ps e =
  List.fold_right
    (fun p e -> <:expr< fun $p$ -> $e$ >>)
    ps
    e

let funs_ids vs e =
  funs (List.map (fun v -> <:patt< $lid:v$ >>) vs) e

let apps e es =
  List.fold_left
    (fun e e' -> <:expr< $e$ $e'$ >>)
    e
    es

let conses es =
  List.fold_right
    (fun e cs -> <:expr< $e$ :: $cs$ >>)
    es
  <:expr< [] >>

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let qual_id name mode id =
  if is_uppercase id.[0]
  then
    match mode with
      | Simple -> <:ident< $uid:id$ >>
      | Modules _ ->
          match id with
            | "exn" -> <:ident< exn >>
            | _ -> <:ident< $uid:name$.$uid:id$ >>
  else
    match mode with
      | Simple -> <:ident< $lid:id$ >>
      | Modules _ ->
          match id with
            | "exn" -> <:ident< exn >>
            | _ -> <:ident< $uid:name$.$lid:id$ >>

let qual_id_aux name mode id =
  if is_uppercase id.[0]
  then
    match mode with
      | Simple -> <:ident< $uid:name ^ "_aux"$.$uid:id$ >>
      | Modules _ -> <:ident< $uid:name$.$uid:id$ >>
  else
    match mode with
      | Simple -> <:ident< $uid:name ^ "_aux"$.$lid:id$ >>
      | Modules _ -> <:ident< $uid:name$.$lid:id$ >>

let gen_type qual_id t =

  let rec gt = function
    | Unit _ -> <:ctyp< unit >>
    | Int _ -> <:ctyp< int >>
    | Int32 _ -> <:ctyp< int32 >>
    | Int64 _ -> <:ctyp< int64 >>
    | Float _ -> <:ctyp< float >>
    | Bool _ -> <:ctyp< bool >>
    | Char _ -> <:ctyp< char >>
    | String _ -> <:ctyp< string >>

    | Var (_, v) -> <:ctyp< '$v$ >>

    | Tuple (_, parts) ->
        let parts = List.map gt parts in
        TyTup (_loc, tySta_of_list parts)

    | Record (_, fields) ->
        let fields =
          List.map
            (fun f ->
              if f.f_mut
              then <:ctyp< $lid:f.f_id$ : mutable $gt f.f_typ$ >>
              else <:ctyp< $lid:f.f_id$ : $gt f.f_typ$ >>)
            fields in
        <:ctyp< { $tySem_of_list fields$ } >>

    | Variant (_, arms) ->
        let arms =
          List.map
            (fun (id, ts) ->
              let parts = List.map gt ts in
              match parts with
                | [] -> <:ctyp< $uid:id$ >>
                | _ -> <:ctyp< $uid:id$ of $tyAnd_of_list parts$ >>)
          arms in
        TySum (_loc, tyOr_of_list arms)

    | Array (_, t) -> <:ctyp< $gt t$ array >>
    | List (_, t) -> <:ctyp< $gt t$ list >>
    | Option (_, t) -> <:ctyp< $gt t$ option >>
    | Ref (_, t) -> <:ctyp< $gt t$ ref >>

    | Apply (_, mdl, id, args) ->
        List.fold_left
          (fun t a -> <:ctyp< $gt a$ $t$ >>)
          (match mdl with
            | None -> <:ctyp< $id:qual_id id$ >>
            | Some mdl -> <:ctyp< $uid:mdl$.$lid:id$ >>)
          args

    | Arrow _ -> assert false in

  gt t

let args_funs args e =
  let ps =
    List.mapi
      (fun a i ->
        let p = <:patt< $lid:"x" ^ string_of_int i$ >> in
        match a with
          | Unlabelled _ -> p
          | Labelled (_, label, _) -> PaLab (_loc, label, p)
          | Optional (_, label, _) -> PaOlb (_loc, label, p))
      args in
  funs ps e

let args_apps e args =
  let es =
    List.mapi
      (fun a i ->
        let e = <:expr< $lid:"x" ^ string_of_int i$ >> in
        match a with
          | Unlabelled _ -> e
          | Labelled (_, label, _) -> ExLab (_loc, label, e)
          | Optional (_, label, _) -> ExOlb (_loc, label, e))
      args in
  apps e es

let args_arrows qual_id args t =
  let ts =
    List.map
      (fun a ->
        let t = gen_type qual_id (typ_of_argtyp a) in
        match a with
          | Unlabelled _ -> t
          | Labelled (_, label, _) -> TyLab (_loc, label, t)
          | Optional (_, label, _) -> TyOlb (_loc, label, t))
      args in
  arrows ts t
