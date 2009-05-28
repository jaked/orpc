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

let to_ id = "to_" ^ id
let long_to_ id = "orpc_js_aux_to_" ^ id

let s_arg id = id ^ "'arg"

let gen_sig_typedef ?(qual_id=G.id) ds =
  <:sig_item< $list:
    List.map
      (fun { td_vars = vars; td_id = id } ->
        let appd =
          G.tapps <:ctyp< $id:qual_id id$ >> (G.tvars vars) in

          <:sig_item<
            val $lid:long_to_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< Orpc_js_server.obj -> '$lid:v$ >>) vars)
                <:ctyp< Orpc_js_server.obj -> $appd$ >>$
              >>)
      ds$ >>

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
        val $lid:to_ aid$ : Orpc_js_server.obj -> $t$
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
    $list:List.map (gen_sig_typedef ~qual_id) typedefs$ ;;
    $if has_excs
     then
       <:sig_item<
         val $lid:long_to_ "exn"$ : Orpc_js_server.obj -> exn
       >>
     else <:sig_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
  >>

let rec gen_to qual_id t x =
  let gen_to = gen_to qual_id in
  match t with
    | Abstract _ -> assert false

    | Var (_loc, id) -> <:expr< $lid:G.to_p id$ $x$ >>

    | Unit _loc -> <:expr< Orpc_js_server.to_unit $x$ >>
    | Int _loc -> <:expr< Orpc_js_server.to_int $x$ >>
    | Int32 _loc -> <:expr< Orpc_js_server.to_int32 $x$ >>
    | Int64 _loc -> <:expr< Orpc_js_server.to_int64 $x$ >>
    | Float _loc -> <:expr< Orpc_js_server.to_float $x$ >>
    | Bool _loc -> <:expr< Orpc_js_server.to_bool $x$ >>
    | Char _loc -> <:expr< Orpc_js_server.to_char $x$ >>
    | String _loc -> <:expr< Orpc_js_server.to_string $x$ >>
    | Tuple (_loc, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr<
          match $x$ with
            | Orpc_js_server.Oblock (0, [| $list:pps$ |]) -> ( $exCom_of_list (List.map2 gen_to parts pes)$ )
            | _ -> raise (Invalid_argument "tuple")
        >>

    | Record (_loc, fields) ->
        let (fps, fes) = G.vars fields in
        let rb f e = <:rec_binding< $id:qual_id f.f_id$ = $gen_to f.f_typ e$ >> in
        <:expr<
          match $x$ with
            | Orpc_js_server.Oblock (0, [| $list:fps$ |]) ->
                $ExRec(_loc, rbSem_of_list (List.map2 rb fields fes), <:expr< >>)$
            | _ -> raise (Invalid_argument "record")
        >>

     | Variant (_loc, arms) ->
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

     | Array (_loc, t) ->
         <:expr< Array.map (fun x -> $gen_to t <:expr< x >>$) $x$ >>

     | List (_loc, t) ->
         <:expr< Orpc_js_server.to_list (fun x -> $gen_to t <:expr< x >>$) $x$ >>

     | Option (_loc, t) ->
         <:expr< Orpc_js_server.to_option (fun x -> $gen_to t <:expr< x >>$) $x$ >>

     | Ref (_loc, t) -> gen_to t <:expr< ! $x$ >>

     | Apply (_loc, mdl, id, args) ->
         <:expr<
           $G.apps
             (<:expr< $id:G.module_id mdl (long_to_ id)$ >>)
             (List.map (fun a -> <:expr< fun x -> $gen_to a <:expr< x >>$ >>) args)$
           $x$
         >>

     | Arrow _ -> assert false

let gen_str_typedef ?(qual_id=G.id) stub ds =
  <:str_item<
    let rec
      $list:
        List.map
          (fun { td_vars = vars; td_id = id; td_typ = t } ->
            <:binding<
              $lid:long_to_ id$ =
              $G.funs_ids
                (List.map G.to_p vars)
                <:expr< fun x -> $if stub then <:expr< assert false >> else gen_to qual_id t <:expr< x >>$ >>$
            >>)
          ds$
  >>

let gen_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

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
        let $lid:to_ aid$ x = $gen_to qual_id arg <:expr< x >>$
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
    $list:List.map (gen_str_typedef ~qual_id false) typedefs$ ;;
    $if has_excs
     then
       let t = Variant (_loc, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item< let $lid:long_to_ "exn"$ x = $gen_to qual_id t <:expr< x >>$ >>
     else <:str_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
  >>
