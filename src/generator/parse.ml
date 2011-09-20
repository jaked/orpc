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
        | <:ctyp< $t1$; $t2$ >> -> fields t1 @ fields t2
        | <:ctyp< $lid:id$ : mutable $t$ >> -> [ { f_id = id; f_mut = true; f_typ = parse_type t } ]
        | <:ctyp< $lid:id$ : $t$ >> -> [ { f_id = id; f_mut = false; f_typ = parse_type t } ]
        | t -> ctyp_error t "expected TySem or TyCol" in
      Record (loc, fields fs)

    (* syntax for TySum? *)
    | TySum (loc, ams) ->
        let rec arms = function
          | <:ctyp< $t1$ | $t2$ >> -> arms t1 @ arms t2
          | <:ctyp< $uid:id$ of $t$ >> ->
              let rec parts = function
                | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
                | t -> [ parse_type t ] in
              [ id, parts t ]
          | <:ctyp< $uid:id$ >> -> [ id, [] ]
          | t -> ctyp_error t "expected TyOr, TyOf, or TyId" in
        Variant (loc, arms ams)

    | TyVrnEq (loc, ams) | TyVrnInf (loc, ams) | TyVrnSup (loc, ams) ->
        let rec arms = function
          | <:ctyp< $t1$ | $t2$ >> -> arms t1 @ arms t2
          | <:ctyp< `$id$ of $t$ >> ->
              let rec parts = function
                | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
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

    | <:ctyp@loc< array $t$ >> -> Array (loc, parse_type t)
    | <:ctyp@loc< list $t$ >> -> List (loc, parse_type t)
    | <:ctyp@loc< option $t$ >> -> Option (loc, parse_type t)
    | <:ctyp@loc< ref $t$ >> -> Ref (loc, parse_type t)

    | <:ctyp@loc< $_$ $_$ >> ->
        let rec apps args = function
            (* TyApp is used for both tupled and nested type application *)
          |  <:ctyp< $t1$ $t2$ >> -> apps (parse_type t2 :: args) t1

          | <:ctyp< $id:id$ >> ->
              let (mdl, id) = parse_ident id in
              Apply (loc, mdl, id, args)

          | t -> ctyp_error t "expected TyApp or TyId" in
        apps [] t

    |  <:ctyp@loc< $t1$ -> $t2$ >> -> Arrow (loc, parse_type t1, parse_type t2)

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
    | <:ctyp< $uid:id$ of $t$ >> ->
      let rec parts = function
        | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
        | t -> [ parse_type t ] in
      (loc, id, parts t )
    | <:ctyp< $uid:id$ >> -> (loc, id, [])
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

type s = {
  typedefs : typedefs list;
  exceptions : exc list;
  module_types : module_type list;
}

let rec parse_sig_items i a =
  match i with
    | SgNil _ -> a
    | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
    | SgTyp (loc, t) -> { a with typedefs = parse_typedef loc t :: a.typedefs }
    | SgExc (loc, t) -> { a with exceptions = parse_exception loc t :: a.exceptions }
    | SgMty (loc, id, MtSig (_, i)) -> { a with module_types = parse_module_type loc id i :: a.module_types }
    | <:sig_item@loc< module type Sync = Abstract with type _r 'a = 'a >> ->
        { a with module_types = { mt_loc = loc; mt_kind = Sync; mt_funcs = With } :: a.module_types }
    | <:sig_item@loc< module type Lwt = Abstract with type _r 'a = Lwt.t 'a >> ->
        { a with module_types = { mt_loc = loc; mt_kind = Lwt; mt_funcs = With } :: a.module_types }
    | _ -> sig_item_error i "expected type, exception, or module type"

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
      | <:sig_item< type _r 'a >> ->
          if mt.mt_kind <> Ik_abstract
          then sig_item_error i "return type may not be declared for non-Abstract module type";
          seen_return_type := true;
          mt
      | i -> sig_item_error i "expected function declaration" in
  let kind =
    match id with
      | "Abstract" -> Ik_abstract
      | "Sync" -> Sync
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
  let s = { typedefs = []; exceptions = []; module_types = [] } in
  let s = parse_sig_items i s in
  let { typedefs = typedefs; exceptions = excs; module_types = mts } = s in
  if List.for_all (function { mt_kind = Ik_abstract } -> true | _ -> false) mts
  then loc_error Loc.ghost "must declare at least one non-Abstract module type";

  (* Let's erase all the primitive type aliases.  We could probably erase *all*
     aliases, but we only need the primitives.  Specifically, we must do it for
     string aliases used in arrays and lists: ocamlnet treats xdr string arrays
     special, so gen_aux generates different code for string arrays and lists.
  *)
  let is_primitive { td_typ } =
    match td_typ with
      | Unit   _
      | Int    _
      | Float  _
      | Bool   _
      | Char   _
      | String _ -> true
      | _        -> false
  in

  (* The table for holding the alias mappings *)
  let aliases = Hashtbl.create 10 in

  (* Map every type. This lets us remove the typedef for the alias entirely.
     The generator won't create xdr conversion functions, so we'll wind up
     with a compilation error if we missed a case *)
  let rec typ_mapper = function
    | Tuple (loc, typs) -> Tuple (loc, List.map typ_mapper typs)
    | Record (loc, fields) ->
        let fields =
          List.map (fun f -> { f with f_typ = typ_mapper f.f_typ} ) fields
        in
        Record (loc, fields)
    | Variant (loc, arms) ->
        let arm_mapper (ident, typs) =
          (ident, List.map typ_mapper typs)
        in
        Variant (loc, List.map arm_mapper arms)
    | PolyVar (loc, kind, arms) ->
        let arm_mapper = function
          | Pv_of (ident, typs) -> Pv_of (ident, List.map typ_mapper typs)
          | Pv_pv typ           -> Pv_pv (typ_mapper typ)
        in
        PolyVar (loc, kind, List.map arm_mapper arms)
    | Array (loc, typ) -> Array (loc, typ_mapper typ)
    | List  (loc, typ) -> List  (loc, typ_mapper typ)
    | Option (loc, typ) -> Option (loc, typ_mapper typ)
    | Ref (loc, typ) -> Ref (loc, typ_mapper typ)
    | Apply (loc, idents, ident, typs) ->
        (try
           let alias = Hashtbl.find aliases ident  in
           alias.td_typ
         with Not_found ->
           Apply (loc, idents, ident, List.map typ_mapper typs)
        )
    | Arrow (loc, typ1, typ2) -> Arrow (loc, typ_mapper typ1, typ_mapper typ2)
    | r -> r
  in  (* let rec typ_mapper *)

  (* Build a table of aliases AND remove them from the typedefs *)
  let rec loop typedefs =
    (* Sweep out the aliases table *)
    if Hashtbl.length aliases > 0 then
      List.iter (fun { td_id } -> Hashtbl.remove aliases td_id) typedefs;

    (* Map all aliased types *)
    let typedefs =
      if Hashtbl.length aliases > 0 then
        List.map
          (fun typedef -> { typedef with td_typ = typ_mapper typedef.td_typ })
          typedefs
      else
        typedefs
    in

    (* Partition the list to pull out the aliased ones *)
    match List.partition is_primitive typedefs with
      | [], rest -> rest
      | primitives, rest ->
          List.iter (fun typedef -> Hashtbl.replace aliases typedef.td_id typedef) primitives;
          loop rest
  in

  (* Map over the lists of types, possibly recursing on each list, to track and remove
     primitive aliases *)
  let typedefs =
    List.filter
      (function
        | [] -> false
        | _  -> true
      )
      (List.map loop typedefs)
  in

  (* Map over the modules to replace aliases with the primitive type *)
  let mts =
    if Hashtbl.length aliases = 0 then
      mts  (* fast exit when nothing to do *)
    else (

      (* Mappers for walking the module record *)
      let argtype_mapper = function
        | Unlabelled (loc, typ) -> Unlabelled (loc, typ_mapper typ)
        | Labelled   (loc, ident, typ) -> Labelled (loc, ident, typ_mapper typ)
        | Optional   (loc, ident, typ) -> Optional (loc, ident, typ_mapper typ)
      in

      let func_mapper (loc, ident, argtypes, typ) =
        (loc, ident, List.map argtype_mapper argtypes, typ_mapper typ)
      in

      (* Map all modules *)
      List.map
        (fun mt ->
          let mt_funcs =
            match mt.mt_funcs with
              | With -> mt.mt_funcs
              | Explicit funcs -> Explicit (List.map func_mapper funcs)
          in
          { mt with
            mt_funcs = mt_funcs;
          }
        )
        mts
    )
  in  (* let mts = *)
  (typedefs, excs, mts)
