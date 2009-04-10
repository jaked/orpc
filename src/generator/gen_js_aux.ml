(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
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
    <:sig_item<
      type
        $list:
          List.map
            (fun { td_vars = vars; td_id = id; td_typ = t; td_eq = eq } ->
              let t = G.gen_type qual_id t in
              let t =
                match eq with
                  | Some eq -> TyMan (_loc, TyId (_loc, eq), t)
                  | None -> t in
              TyDcl (_loc, id, G.tvars vars, t, []))
            ds$
    >> in

  let gen_typedef_funs ds =
    <:sig_item<
      $list:
        List.map
          (fun { td_vars = vars; td_id = id } ->
            let appd =
              G.tapps <:ctyp< $id:qual_id id$ >> (G.tvars vars) in

            <:sig_item<
              val $lid:G.to_ id$ :
                $G.arrows
                  (List.map (fun v -> <:ctyp< Orpc_js_server.obj -> '$lid:v$ >>) vars)
                  <:ctyp< Orpc_js_server.obj -> $appd$ >>$
          >>)
          ds$
    >> in

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
        val $lid:G.to_ aid$ : Orpc_js_server.obj -> $t$
      >> in
    <:sig_item<
      $items (s_arg id) arg$
    >> in

  <:sig_item<
    $match mode with
      | Simple ->
          <:sig_item<
            $list:List.map gen_typedef_typs typedefs$ ;;
            $list:List.map gen_exc excs$ ;;
          >>
      | _ -> <:sig_item< >>$ ;;
    $list:List.map gen_typedef_funs typedefs$ ;;
    $if has_excs
     then
       <:sig_item<
         val to_exn : Orpc_js_server.obj -> exn
       >>
     else <:sig_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
  >>

let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

  let rec gen_to t x =
    match t with
      | Var (_, id) -> <:expr< $lid:G.to_p id$ $x$ >>
      | Unit _ -> <:expr< Orpc_js_server.to_unit $x$ >>
      | Int _ -> <:expr< Orpc_js_server.to_int $x$ >>
      | Int32 _ -> <:expr< Orpc_js_server.to_int32 $x$ >>
      | Int64 _ -> <:expr< Orpc_js_server.to_int64 $x$ >>
      | Float _ -> <:expr< Orpc_js_server.to_float $x$ >>
      | Bool _ -> <:expr< Orpc_js_server.to_bool $x$ >>
      | Char _ -> <:expr< Orpc_js_server.to_char $x$ >>
      | String _ -> <:expr< Orpc_js_server.to_string $x$ >>
      | Tuple (_, parts) ->
          let (pps, pes) = G.vars parts in
          <:expr<
            match $x$ with
              | Orpc_js_server.Oblock (0, [| $list:pps$ |]) -> ( $exCom_of_list (List.map2 gen_to parts pes)$ )
              | _ -> raise (Invalid_argument "tuple")
          >>

      | Record (_, fields) ->
          let (fps, fes) = G.vars fields in
          let rb f e = <:rec_binding< $id:qual_id f.f_id$ = $gen_to f.f_typ e$ >> in
          <:expr<
            match $x$ with
              | Orpc_js_server.Oblock (0, [| $list:fps$ |]) ->
                  $ExRec(_loc, rbSem_of_list (List.map2 rb fields fes), <:expr< >>)$
              | _ -> raise (Invalid_argument "record")
          >>

       | Variant (_, arms) ->
           let mc (id, ts) i =
             match ts with
               | [] ->
                   <:match_case< Orpc_js_server.Oint $`int:i$ -> $id:qual_id id$ >>
               | [t] ->
                   <:match_case< Orpc_js_server.Oblock ($`int:i$, [| x |]) -> $id:qual_id id$ $gen_to t <:expr< x >>$ >>
               | _ ->
                   let (pps, pes) = G.vars ts in
                   <:match_case<
                     Orpc_js_server.Oblock ($`int:i$, [| $list:pps$ |]) ->
                       $List.fold_left
                         (fun ps p -> <:expr< $ps$ $p$ >>)
                         <:expr< $id:qual_id id$ >>
                         (List.map2 gen_to ts pes)$
                   >> in
           <:expr<
             match $x$ with
               | Orpc_js_server.Oint _ ->
                   (match $x$ with
                       $list:
                         List.mapi
                           mc
                           (List.filter (fun (_, ts) -> ts = []) arms)$
                     | _ -> raise (Invalid_argument "variant"))
               | Orpc_js_server.Oblock _ ->
                   (match $x$ with
                       $list:
                         List.mapi
                           mc
                           (List.filter (fun (_, ts) -> ts <> []) arms)$
                     | _ -> raise (Invalid_argument "variant"))
               | _ -> raise (Invalid_argument "variant")
           >>

       | Array (_, t) ->
           <:expr< Array.map (fun x -> $gen_to t <:expr< x >>$) $x$ >>

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
    <:str_item<
      type
        $list:
          List.map
            (fun { td_vars = vars; td_id = id; td_typ = t; td_eq = eq } ->
              let vars = List.map (fun v -> <:ctyp< '$lid:v$ >>) vars in
              let t = G.gen_type qual_id t in
              let t =
                match eq with
                  | Some eq -> TyMan (_loc, TyId (_loc, eq), t)
                  | None -> t in
              TyDcl (_loc, id, vars, t, []))
          ds$
    >> in

  let gen_typedef_funs ds =
    <:str_item<
      let rec
        $list:
          List.map
            (fun { td_vars = vars; td_id = id; td_typ = t } ->
              <:binding<
                $lid:G.to_ id$ =
                $G.funs_ids
                  (List.map G.to_p vars)
                  <:expr< fun x -> $gen_to t <:expr< x >>$ >>$
              >>)
           ds$
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
            $list:List.map gen_typedef_typs typedefs$ ;;
            $list:List.map gen_exc excs$ ;;
          >>
      | _ -> <:str_item< >>$ ;;
    $list:List.map gen_typedef_funs typedefs$ ;;
    $if has_excs
     then
       let t = Variant (_loc, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item< let to_exn x = $gen_to t <:expr< x >>$ >>
     else <:str_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
  >>
