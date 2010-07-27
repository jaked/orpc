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

let _loc = Camlp4.PreCast.Loc.ghost

module G = Gen_common

let s_arg id = id ^ "'arg"
let s_res id = id ^ "'res"

let long_to_ id = "orpc_js_aux_to_" ^ id
let long_of_ id = "orpc_js_aux_of_" ^ id

let to_ id = "to_" ^ id
let of_ id = "of_" ^ id

(* from btype.ml in the OCaml source *)
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

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

            val $lid:long_of_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< '$lid:v$ -> Orpc_js_server.obj >>) vars)
                (TyArr (_loc, appd, <:ctyp< Orpc_js_server.obj >>))
                (* <:ctyp< $appd$ -> Orpc_js_server.obj >> broken in 3.12 *)$
              >>)
      ds$ >>

let gen_mli name (typedefs, excs, funcs, kinds) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name in

  let gen_func (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (g, args) in
    let orpc_res =
      if has_excs
      then Apply (_loc, ["Orpc"], "orpc_result", [res; Apply (_loc, [], "exn", [])])
      else res in
    let items aid arg =
      let t = G.gen_type qual_id arg in
      <:sig_item<
        val $lid:to_ aid$ : Orpc_js_server.obj -> $t$
        val $lid:of_ aid$ : $t$ -> Orpc_js_server.obj
      >> in
    <:sig_item<
      $items (s_arg id) arg$
      $items (s_res id) orpc_res$
    >> in

  <:sig_item<
    $list:List.map (gen_sig_typedef ~qual_id) typedefs$ ;;
    $if has_excs
     then
       <:sig_item<
         val $lid:long_to_ "exn"$ : Orpc_js_server.obj -> exn
         val $lid:long_of_ "exn"$ : exn -> Orpc_js_server.obj
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
                <:match_case< Orpc_js_server.Onumber $`flo:float_of_int i$ -> $id:qual_id id$ >>
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
            | $list:List.mapi mc (List.filter (fun (_, ts) -> ts = []) arms)$
            | $list:List.mapi mc (List.filter (fun (_, ts) -> ts <> []) arms)$
            | _ -> raise (Invalid_argument "variant")
        >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let mc (id, ts) =
          match ts with
            | [] ->
                <:match_case<
                  Orpc_js_server.Onumber $`flo:float_of_int (hash_variant id)$ -> `$id$
                >>
            | _ ->
                let t = match ts with [t] -> t | _ -> Tuple (_loc, ts) in
                <:match_case<
                  Orpc_js_server.Oblock (0, [| Orpc_js_server.Onumber $`flo:float_of_int (hash_variant id)$; x |]) ->
                    `$id$ $gen_to t <:expr< x >>$
                >> in
        <:expr<
          match $x$ with
            | $list:List.map mc arms$
            | _ -> raise (Invalid_argument "polyvar")
        >>

    | Array (_loc, t) ->
        <:expr< Orpc_js_server.to_array (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | List (_loc, t) ->
        <:expr< Orpc_js_server.to_list (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | Option (_loc, t) ->
        <:expr< Orpc_js_server.to_option (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | Ref (_loc, t) ->
        <:expr< Orpc_js_server.to_ref (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | Apply (_loc, mdl, id, args) ->
        <:expr<
          $G.apps
            (<:expr< $id:G.module_id mdl (long_to_ id)$ >>)
            (List.map (fun a -> <:expr< fun x -> $gen_to a <:expr< x >>$ >>) args)$
          $x$
        >>

    | Arrow _ -> assert false

let rec gen_of qual_id t v =
  let gen_of = gen_of qual_id in
  match t with
    | Abstract _ -> assert false

    | Var (_loc, id) -> <:expr< $lid:G.of_p id$ $v$ >>

    | Unit _loc -> <:expr< Orpc_js_server.of_unit $v$ >>
    | Int _loc -> <:expr< Orpc_js_server.of_int $v$ >>
    | Int32 _loc -> <:expr< Orpc_js_server.of_int32 $v$ >>
    | Int64 _loc -> <:expr< Orpc_js_server.of_int64 $v$ >>
    | Float _loc -> <:expr< Orpc_js_server.of_float $v$ >>
    | Bool _loc -> <:expr< Orpc_js_server.of_bool $v$ >>
    | Char _loc -> <:expr< Orpc_js_server.of_char $v$ >>
    | String _loc -> <:expr< Orpc_js_server.of_string $v$ >>

    | Tuple (_loc, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr<
          let ( $tup:paCom_of_list pps$ ) = $v$ in
          Orpc_js_server.Oblock (0, [| $exSem_of_list (List.map2 gen_of parts pes)$ |]) (* XXX not sure why list: doesn't work here *)
        >>

    | Record (_loc, fields) ->
        let (fps, fes) = G.vars fields in
        let rb f p = <:patt< $id:qual_id f.f_id$ = $p$ >> in
        <:expr<
          let { $paSem_of_list (List.map2 rb fields fps)$ } = $v$ in
          Orpc_js_server.Oblock (0,
            [| $exSem_of_list (List.map2 (fun f v -> gen_of f.f_typ v) fields fes)$ |])
        >>

    | Variant (_loc, arms) ->
        let mc (id, ts) i =
          match ts with
            | [] ->
                <:match_case< $id:qual_id id$ -> Orpc_js_server.Onumber $`flo:float_of_int i$ >>
            | _ ->
                let (pps, pes) = G.vars ts in
                <:match_case<
                  $G.papps <:patt< $id:qual_id id$ >> pps$ ->
                    Orpc_js_server.Oblock ($`int:i$, [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                >> in
        <:expr<
          match $v$ with
            | $list:List.mapi mc (List.filter (fun (_, ts) -> ts = []) arms)$
            | $list:List.mapi mc (List.filter (fun (_, ts) -> ts <> []) arms)$
        >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let mc (id, ts) =
          match ts with
            | [] ->
                <:match_case<
                  `$id$ -> Orpc_js_server.Onumber $`flo:float_of_int (hash_variant id)$
                >>
            | _ ->
                let t = match ts with [t] -> t | _ -> Tuple (_loc, ts) in
                <:match_case<
                  `$id$ x ->
                    Orpc_js_server.Oblock (0, [| Orpc_js_server.Onumber $`flo:float_of_int (hash_variant id)$; $gen_of t <:expr< x >>$ |])
                >> in
        <:expr< match $v$ with $list:List.map mc arms$ >>

    | Array (_loc, t) ->
        <:expr< Orpc_js_server.of_array (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | List (_loc, t) ->
        <:expr< Orpc_js_server.of_list (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | Option (_loc, t) ->
        <:expr< Orpc_js_server.of_option (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | Ref (_loc, t) ->
        <:expr< Orpc_js_server.of_ref (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | Apply (_loc, mdl, id, args) ->
        <:expr<
          $G.apps
            (<:expr< $id:G.module_id mdl (long_of_ id)$ >>)
            (List.map (fun a -> <:expr< fun v -> $gen_of a <:expr< v >>$ >>) args)$
          $v$
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

let gen_ml name (typedefs, excs, funcs, kinds) =
  let has_excs = excs <> [] in
  let qual_id = G.qual_id name in

  let gen_of_exc t v =
    match gen_of qual_id t v with
      | ExMat (loc, e, cases) ->
          ExMat (loc, e, McOr(_loc, cases, <:match_case< _ -> raise $v$ >>))
      | _ -> assert false in

  let gen_func (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (_loc, args) in
    let orpc_res =
      if has_excs
      then Apply (_loc, ["Orpc_js_server"], "orpc_result", [res; Apply (_loc, [], "exn", [])])
      else res in
    let items aid arg =
      <:str_item<
        let $lid:to_ aid$ x = $gen_to qual_id arg <:expr< x >>$
        let $lid:of_ aid$ v = $gen_of qual_id arg <:expr< v >>$
      >> in
    <:str_item<
      $items (s_arg id) arg$ ;;
      $items (s_res id) orpc_res$ ;;
    >> in

  <:str_item<
    $list:List.map (gen_str_typedef ~qual_id false) typedefs$ ;;
    $if has_excs
     then
       let t = Variant (_loc, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item<
         let $lid:long_to_ "exn"$ x = $gen_to qual_id t <:expr< x >>$ ;;
         let $lid:long_of_ "exn"$ v = $gen_of_exc t <:expr< v >>$ ;;
       >>
     else <:str_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
  >>
