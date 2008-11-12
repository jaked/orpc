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

module G = Gen_common

let s_arg id = id ^ "'arg"

let gen_mli name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

  let gen_typedef_typs ds =
    SgTyp (_loc,
          tyAnd_of_list
            (List.map
                (fun { td_vars = vars; td_id = id; td_typ = t; td_eq = eq } ->
                  let t = G.gen_type qual_id t in
                  let t =
                    match eq with
                      | Some eq -> TyMan (_loc, TyId (_loc, eq), t)
                      | None -> t in
                  TyDcl (_loc, id, G.tvars vars, t, []))
                ds)) in

  let gen_typedef_funs ds =
    sgSem_of_list
      (List.map
          (fun { td_vars = vars; td_id = id } ->
            let appd =
              G.tapps <:ctyp< $id:qual_id id$ >> (G.tvars vars) in

            <:sig_item<
              val $lid:G.to_ id$ :
                $G.arrows
                  (List.map (fun v -> <:ctyp< Obj.t -> '$lid:v$ >>) vars)
                  <:ctyp< Obj.t -> $appd$ >>$
          >>)
          ds) in

  let gen_exc (_, id, ts) =
    <:sig_item<
      exception $uid:id$ of $tyAnd_of_list (List.map (G.gen_type qual_id) ts)$
    >> in

  let gen_func (_, id, args, _) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (g, args) in
    let items aid arg =
      let t = G.gen_type qual_id arg in
      <:sig_item<
        val $lid:G.to_ aid$ : Obj.t -> $t$
      >> in
    <:sig_item<
      $items (s_arg id) arg$
    >> in

  <:sig_item<
    $match mode with
      | Simple ->
          <:sig_item<
            $sgSem_of_list (List.map gen_typedef_typs typedefs)$ ;;
            $sgSem_of_list (List.map gen_exc excs)$ ;;
          >>
      | _ -> <:sig_item< >>$ ;;
    $sgSem_of_list (List.map gen_typedef_funs typedefs)$ ;;
    $if has_excs
     then
       <:sig_item<
         val to_exn : Obj.t -> exn
       >>
     else <:sig_item< >>$ ;;
    $sgSem_of_list (List.map gen_func funcs)$ ;;
  >>

let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

  let rec gen_to t x =
    match t with
      | Var (_, id) -> <:expr< $lid:G.to_p id$ $x$ >>
      | Unit _ ->
          <:expr<
            if Obj.is_int $x$ && Obj.obj $x$ = 0
            then ()
            else raise (Invalid_argument "unit")
          >>
      | Int _ ->
          <:expr<
            if Obj.is_int $x$
            then Obj.obj $x$
            else raise (Invalid_argument "int")
          >>
      | Int32 _ ->
          <:expr<
            if Obj.is_int $x$
            then Int32.of_int (Obj.obj $x$)
            else if Obj.tag $x$ = Obj.double_tag
            then Int32.of_float (Obj.obj $x$)
            else raise (Invalid_argument "int32")
          >>
      | Int64 _ ->
          <:expr<
            if Obj.is_int $x$
            then Int64.of_int (Obj.obj $x$)
            else if Obj.tag $x$ = Obj.double_tag
            then Int64.of_float (Obj.obj $x$)
            else raise (Invalid_argument "int64")
          >>
      | Float _ ->
          <:expr<
            if Obj.is_int $x$
            then float_of_int (Obj.obj $x$)
            else if Obj.tag $x$ = Obj.double_tag
            then Obj.obj $x$
            else raise (Invalid_argument "float")
          >>
      | Bool _ ->
          <:expr<
            if Obj.is_int $x$ && (Obj.obj $x$ : int) = 0
            then match Obj.obj $x$ with
              | 0 -> false
              | 1 -> true
              | _ -> raise (Invalid_argument "unit")
            else raise (Invalid_argument "unit")
          >>
      | Char _ ->
          <:expr<
            if Obj.is_int $x$
            then char_of_int (Obj.obj $x$)
            else raise (Invalid_argument "char")
          >>
      | String _ ->
          <:expr<
            if not (Obj.is_int $x$) && Obj.tag $x$ = Obj.string_tag
            then Obj.obj $x$
            else raise (Invalid_argument "string")
          >>

      | Tuple (_, parts) ->
          <:expr<
            if Obj.is_int $x$ || Obj.tag $x$ <> 0 || Obj.size $x$ <> $`int:List.length parts$
            then raise (Invalid_argument "tuple")
            else ( $exCom_of_list (List.mapi (fun p i -> gen_to p <:expr< Obj.field $x$ $`int:i$ >>) parts)$ )
          >>

      | Record (_, fields) ->
          let rb f e = <:rec_binding< $id:qual_id f.f_id$ = $gen_to f.f_typ e$ >> in
          <:expr<
            if Obj.is_int $x$ || Obj.tag $x$ <> 0 || Obj.size $x$ <> $`int:List.length fields$
            then raise (Invalid_argument "record")
            else $ExRec(_loc, rbSem_of_list (List.mapi (fun f i -> rb f <:expr< Obj.field $x$ $`int:i$>>) fields), <:expr< >>)$
          >>

       | Variant (_, arms) ->
           let mc (id, ts) i =
             match ts with
               | [] -> <:match_case< $`int:i$ -> $id:qual_id id$ >>
               | [t] ->
                   <:match_case<
                     $`int:i$ ->
                       if Obj.size $x$ <> 1
                       then raise (Invalid_argument "variant")
                       else $id:qual_id id$ $gen_to t <:expr< Obj.field $x$ 0 >>$
                   >>
               | _ ->
                   <:match_case<
                     $`int:i$ ->
                       if Obj.size $x$ <> $`int:List.length ts$
                       then raise (Invalid_argument "variant")
                       else
                         $List.fold_left
                           (fun ps p -> <:expr< $ps$ $p$ >>)
                           <:expr< $id:qual_id id$ >>
                           (List.mapi (fun t i -> gen_to t <:expr< Obj.field $x$ $`int:i$ >>) ts)$
                   >> in
           <:expr<
             if Obj.is_int $x$
             then
               $ExMat (_loc,
                      <:expr< Obj.obj $x$ >>,
                      mcOr_of_list
                        (List.mapi
                            mc
                            (List.filter (fun (_, ts) -> ts = []) arms) @
                            [ <:match_case< _ -> raise (Invalid_argument "variant") >> ]))$
             else
               $ExMat (_loc,
                      <:expr< Obj.tag $x$ >>,
                      mcOr_of_list
                        (List.mapi
                            mc
                            (List.filter (fun (_, ts) -> ts <> []) arms) @
                            [ <:match_case< _ -> raise (Invalid_argument "variant") >> ]))$
           >>

       | Array (_, t) ->
           <:expr<
             if Obj.is_int $x$ || Obj.tag $x$ <> 0
             then raise (Invalid_argument "array")
             else Array.map (fun x -> $gen_to t <:expr< x >>$) (Obj.obj $x$)
           >>

       | List (_, t) ->
           <:expr< Orpc_js_server.to_list (fun x -> $gen_to t <:expr< x >>$) $x$ >>

       | Option (_, t) ->
           <:expr< Orpc_js_server.to_option (fun x -> $gen_to t <:expr< x >>$) $x$ >>

       | Ref (_, t) -> gen_to t <:expr< ! $x$ >>

       | Apply (_, mdl, id, args) ->
           <:expr<
             $G.apps
               (match mdl with
                 | None -> <:expr< $lid:G.to_ id$ >>
                 | Some mdl -> <:expr< $uid:mdl$ . $lid:G.to_ id$ >>)
               (List.map (fun a -> <:expr< fun x -> $gen_to a <:expr< x >>$ >>) args)$
             $x$
           >>

       | Arrow _ -> assert false in

  let gen_typedef_typs ds =
    StTyp (_loc,
          tyAnd_of_list
            (List.map
                (fun { td_vars = vars; td_id = id; td_typ = t; td_eq = eq } ->
                  let vars = List.map (fun v -> <:ctyp< '$lid:v$ >>) vars in
                  let t = G.gen_type qual_id t in
                  let t =
                    match eq with
                      | Some eq -> TyMan (_loc, TyId (_loc, eq), t)
                      | None -> t in
                  TyDcl (_loc, id, vars, t, []))
                ds)) in

  let gen_typedef_funs ds =
    <:str_item<
      $let es =
         List.map
           (fun { td_vars = vars; td_id = id; td_typ = t } ->
             <:binding<
               $lid:G.to_ id$ =
               $G.funs_ids
                 (List.map G.to_p vars)
                 <:expr< fun x -> $gen_to t <:expr< x >>$ >>$
             >>)
           ds in
       StVal (_loc, BTrue, biAnd_of_list es)$
    >> in

  let gen_exc (_, id, ts) =
    <:str_item<
      exception $uid:id$ of $tyAnd_of_list (List.map (G.gen_type qual_id) ts)$
    >> in

  let gen_func (_, id, args, _) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (_loc, args) in
    let items aid arg =
      <:str_item<
        let $lid:G.to_ aid$ x = $gen_to arg <:expr< x >>$
      >> in
    <:str_item< $items (s_arg id) arg$ >> in

  <:str_item<
    $match mode with
      | Simple ->
          <:str_item<
            $stSem_of_list (List.map gen_typedef_typs typedefs)$ ;;
            $stSem_of_list (List.map gen_exc excs)$ ;;
          >>
      | _ -> <:str_item< >>$ ;;
    $stSem_of_list (List.map gen_typedef_funs typedefs)$ ;;
    $if has_excs
     then
       let t = Variant (_loc, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item< let to_exn x = $gen_to t <:expr< x >>$ >>
     else <:str_item< >>$ ;;
    $stSem_of_list (List.map gen_func funcs)$ ;;
  >>
