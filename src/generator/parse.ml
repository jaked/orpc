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
open Error

let parse_ident id =
  let ids = Ast.list_of_ident id [] in
  match List.rev ids with
    | <:ident< $lid:id$ >> :: uids ->
      let mdl =
        List.map
          (function <:ident< $uid:uid$ >> -> uid | _ -> assert false)
          (List.rev uids) in
      (mdl, id)
    | _ -> assert false

let rec parse_type t =
  match t with
    | <:ctyp@loc< unit >> -> Unit loc
    | <:ctyp@loc< int >> -> Int loc
    | <:ctyp@loc< int32 >> -> Int32 loc
    | <:ctyp@loc< int64 >> -> Int64 loc
    | <:ctyp@loc< float >> -> Float loc
    | <:ctyp@loc< bool >> -> Bool loc
    | <:ctyp@loc< char >> -> Char loc
    | <:ctyp@loc< string >> -> String loc

    | <:ctyp@loc< '$v$ >> -> Var (loc, v)

    | <:ctyp@loc< $id:id$ >> ->
      let (mdl, id) = parse_ident id in
      Apply (loc, mdl, id, [])

    (* I don't see how to do this one with quotations; $t1$ * $t2$
       gives both the TyTup and the TySta *)
    | TyTup (loc, ts) ->
        let rec parts = function
          | TySta (_, t1, t2) -> parts t1 @ parts t2
          | TyTup (_, t) -> parts t
          | t -> [ parse_type t ] in
        Tuple (loc, parts ts)

    | <:ctyp@loc< { $fs$ } >> ->
      let rec fields = function
        | TySem (_loc, t1, t2) -> fields t1 @ fields t2 (* <:ctyp< $t1$; $t2$ >> has extra TySum in 3.12 ?? *)
        | TyCol (_loc, <:ctyp< $lid:id$ >>, <:ctyp< mutable $t$ >>) -> [ { f_id = id; f_mut = true; f_typ = parse_type t } ]
(*        | <:ctyp< $lid:id$ : mutable $t$ >> -> [ { f_id = id; f_mut = true; f_typ = parse_type t } ] broken in 3.12 *)
        | TyCol (_loc, <:ctyp< $lid:id$ >>, t) -> [ { f_id = id; f_mut = false; f_typ = parse_type t } ]
(*        | <:ctyp< $lid:id$ : $t$ >> -> [ { f_id = id; f_mut = false; f_typ = parse_type t } ] broken in 3.12 *)
        | t -> ctyp_error t "expected TySem or TyCol" in
      Record (loc, fields fs)

    (* syntax for TySum? *)
    | TySum (loc, ams) ->
        let rec arms = function
          | TyOr (_, t1, t2) -> arms t1 @ arms t2 (* <:ctyp< $t1$ | $t2$ >> has extra TySum in 3.12 *)
          | TyOf (_, TyId (_, IdUid (_, id)), t) -> (* <:ctyp< $uid:id$ of $t$ >> has extra TySum in 3.12 *)
              let rec parts = function
                | TyAnd (_, t1, t2) -> parts t1 @ parts t2 (* <:ctyp< $t1$ and $t2$ >> has extra TySum in 3.12 ?? *)
                | t -> [ parse_type t ] in
              [ id, parts t ]
          | TyId (_, IdUid (_, id)) -> [ id, [] ] (* <:ctyp< $uid:id$ >> has extra TySum in 3.12 *)
          | t -> ctyp_error t "expected TyOr, TyOf, or TyId" in
        Variant (loc, arms ams)

    | TyVrnEq (loc, ams) | TyVrnInf (loc, ams) | TyVrnSup (loc, ams) ->
        let rec arms = function
          | TyOr (_, t1, t2) -> arms t1 @ arms t2 (* <:ctyp< $t1$ | $t2$ >> has extra TySum in 3.12 *)
          | <:ctyp< `$id$ of $t$ >> ->
              let rec parts = function
                | TyAnd (_, t1, t2) -> parts t1 @ parts t2 (* <:ctyp< $t1$ and $t2$ >> has extra TySum in 3.12 ?? *)
                | t -> [ parse_type t ] in
              [ Pv_of (id, parts t) ]
          | <:ctyp< `$id$ >> -> [ Pv_of (id, []) ]
          | t -> [ Pv_pv (parse_type t) ] in
        let kind = match t with
          | TyVrnEq _ -> Pv_eq
          | TyVrnInf _ -> Pv_inf
          | TyVrnSup _ -> Pv_sup
          | _ -> assert false in
        PolyVar (loc, kind, arms ams)

    | TyApp (loc, <:ctyp< array >>, t) -> Array (loc, parse_type t) (* <:ctyp@loc< $t$ array >> broken in 3.12 *)
    | TyApp (loc, <:ctyp< list >>, t) -> List (loc, parse_type t) (* <:ctyp@loc< $t$ list >> broken in 3.12 *)
    | TyApp (loc, <:ctyp< option >>, t) -> Option (loc, parse_type t) (* <:ctyp@loc< $t$ option >> broken in 3.12 *)
    | TyApp (loc, <:ctyp< ref >>, t) -> Ref (loc, parse_type t) (* <:ctyp@loc< $t$ ref >> broken 3.12 *)

    | TyApp (loc, _, _) -> (* <:ctyp@loc< $_$ $_$ >> broken in 3.12 *)
        let rec apps args = function
            (* TyApp is used for both tupled and nested type application *)
          | TyApp (_, t1, t2) -> apps (parse_type t2 :: args) t1 (* <:ctyp< $t2$ $t1$ >> broken in 3.12 *)

          | <:ctyp< $id:id$ >> ->
              let (mdl, id) = parse_ident id in
              Apply (loc, mdl, id, args)

          | t -> ctyp_error t "expected TyApp or TyId" in
        apps [] t

    | TyArr (loc, t1, t2) -> Arrow (loc, parse_type t1, parse_type t2) (* <:ctyp@loc< $t1$ -> $t2$ >> broken in 3.12 *)

    | t -> ctyp_error t "unsupported type"

let parse_typedef ?(allow_abstract=false) loc t =
  let rec types t a =
    match t with
      | TyAnd (_, t1, t2) -> types t1 (types t2 a)
      | TyDcl (loc, id, tvars, t, []) ->
          let tvars =
            List.map
              (function
                | TyQuo (_, v) -> v
                | t -> ctyp_error t "expected type variable")
              tvars in
          let eq, t =
            match t with
              | TyMan (_, TyId (_, eq), t) -> Some eq, t
              | _ -> None, t in
          let t =
            match t, allow_abstract with
              | TyNil loc, true -> Abstract loc
              | TyNil _, false -> ctyp_error t "abstract type not allowed"
              | _ -> parse_type t in
          { td_loc = loc; td_vars = tvars; td_id = id; td_typ = t; td_eq = eq } ::a
    | t -> ctyp_error t "expected type declaration" in
  types t []

let parse_exception loc t =
  match t with
    | TyOf (_, TyId (_, IdUid (_, id)), t) -> (* <:ctyp< $uid:id$ of $t$ >> has extra TySum in 3.12 *)
      let rec parts = function
        | TyAnd (_, t1, t2) -> parts t1 @ parts t2 (* <:ctyp< $t1$ and $t2$ >> has extra TySum in 3.12 ?? *)
        | t -> [ parse_type t ] in
      (loc, id, parts t )
    | TyId (_, IdUid (_, id)) -> (loc, id, []) (* <:ctyp< $uid:id$ >> has extra TySum in 3.12 *)
    | t -> ctyp_error t "expected TyOr, TyOf, or TyId"

let parse_val loc id t =
  let rec args t a =
    match t with
      | TyArr (_, t1, t2) ->
          let arg =
            match t1 with
              | TyLab (loc, label, t1) -> Labelled (loc, label, parse_type t1)
              | TyOlb (loc, label, t1) -> Optional (loc, label, parse_type t1)
              | _ -> Unlabelled (loc_of_ctyp t1, parse_type t1) in
          args t2 (arg :: a)
      | t -> List.rev a, parse_type t in
  match args t [] with
    | [], _ -> loc_error loc "function must have at least one argument"
    | args, ret -> (loc, id, args, ret)

let wrap_sig_item i =
  (* :sig_item wraps SgSem / SgNil around what it parses, so we must
     wrap the scrutinee when using :sig_item in patterns *)
  let loc = Ast.loc_of_sig_item i in
  SgSem (loc, i, SgNil loc)

type s = {
  typedefs : typedefs list;
  exceptions : exc list;
  funcs : func list;
  module_types : module_type list;
}

let rec parse_sig_items i a =
  match i with
    | SgNil _ -> a
    | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
    | SgTyp (loc, t) -> { a with typedefs = parse_typedef loc t :: a.typedefs }
    | SgExc (loc, t) -> { a with exceptions = parse_exception loc t :: a.exceptions }
    | SgVal (loc, id, t) -> { a with funcs = parse_val loc id t :: a.funcs }
    | SgMty (loc, id, MtSig (_, i)) -> { a with module_types = parse_module_type loc id i :: a.module_types }
    | _ ->
        match wrap_sig_item i with
          | <:sig_item@loc< module type Sync = Abstract with type 'a _r = 'a >> ->
              { a with module_types = { mt_loc = loc; mt_kind = Sync; mt_funcs = With } :: a.module_types }
          | <:sig_item@loc< module type Async = Abstract with type 'a _r = ((unit -> 'a) -> unit) -> unit >> ->
              { a with module_types = { mt_loc = loc; mt_kind = Async; mt_funcs = With } :: a.module_types }
          | <:sig_item@loc< module type Lwt = Abstract with type 'a _r = 'a Lwt.t >> ->
              { a with module_types = { mt_loc = loc; mt_kind = Lwt; mt_funcs = With } :: a.module_types }
          | i -> sig_item_error i "expected type, function declaration, or module type"

and parse_module_type loc id i =
  let seen_return_type = ref false in
  let rec parse_sig_items i mt =
    match i with
      | SgNil _ -> mt
      | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 mt)
      | SgVal (loc, id, t) ->
          begin match mt.mt_funcs with
            | With -> assert false
            | Explicit funcs -> { mt with mt_funcs = Explicit (parse_val loc id t :: funcs) }
          end
      | _ ->
          match wrap_sig_item i with
            | <:sig_item< type 'a _r >> ->
              if mt.mt_kind <> Ik_abstract
              then sig_item_error i "return type may not be declared for non-Abstract module type";
              seen_return_type := true;
              mt
            | i -> sig_item_error i "expected function declaration" in
  let kind =
    match id with
      | "Abstract" -> Ik_abstract
      | "Sync" -> Sync
      | "Async" -> Async
      | "Lwt" -> Lwt
      | _ -> loc_error loc "unknown interface kind" in
  let mt = { mt_loc = loc; mt_kind = kind; mt_funcs = Explicit [] } in
  let mt = parse_sig_items i mt in
  if mt.mt_funcs = Explicit []
  then loc_error mt.mt_loc "must declare at least one function";
  if mt.mt_kind = Ik_abstract && not !seen_return_type
  then loc_error mt.mt_loc "module type Abstract must declare return type";
  mt

let parse_interface i =
  let s = { typedefs = []; exceptions = []; funcs = []; module_types = [] } in
  let s = parse_sig_items i s in
  let { typedefs = typedefs; exceptions = excs; funcs = funcs; module_types = mts } = s in
  match s with
    | { funcs = _::_; module_types = [] } -> (typedefs, excs, funcs, mts) (* simple interface *)
    | { funcs = []; module_types = _::_ } ->
        if List.for_all (function { mt_kind = Ik_abstract } -> true | _ -> false) mts
        then loc_error Loc.ghost "must declare at least one non-Abstract module";
        (typedefs, excs, funcs, mts) (* modules interface *)
    | _ -> loc_error Loc.ghost "expected simple interface or modules interface"
