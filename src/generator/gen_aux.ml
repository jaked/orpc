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

let xdr_p id = "xdr'" ^ id

let long_xdr id = "orpc_aux_xdr_" ^ id
let long_to_ id = "orpc_aux_to_" ^ id
let long_of_ id = "orpc_aux_of_" ^ id

let xdr id = "xdr_" ^ id
let to_ id = "to_" ^ id
let of_ id = "of_" ^ id

let gen_sig_typedef ?(qual_id=G.id) ds =
  <:sig_item< $list:
    List.map
      (fun { td_vars = vars; td_id = id } ->
        let appd =
          G.tapps <:ctyp< $id:qual_id id$ >> (G.tvars vars) in

        <:sig_item<
           val $lid:long_to_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< Xdr.xdr_value -> '$lid:v$ >>) vars)
                <:ctyp< Xdr.xdr_value -> $appd$ >>$

            val $lid:long_of_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< '$lid:v$ -> Xdr.xdr_value >>) vars)
                (TyArr (_loc, appd, <:ctyp< Xdr.xdr_value >>))
                (* <:ctyp< $appd$ -> Xdr.xdr_value >> broken in 3.12 *)$

            val $lid:long_xdr id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< Xdr.xdr_type_term >>) vars)
                <:ctyp< Xdr.xdr_type_term >>$
        >>)
      ds$
  >>

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
        val $lid:to_ aid$ : Xdr.xdr_value -> $t$
        val $lid:of_ aid$ : $t$ -> Xdr.xdr_value
        val $lid:xdr aid$ : Xdr.xdr_type_term
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
         val $lid:long_to_ "exn"$ : Xdr.xdr_value -> exn
         val $lid:long_of_ "exn"$ : exn -> Xdr.xdr_value
         val $lid:long_xdr "exn"$ : Xdr.xdr_type_term ;;
       >>
     else <:sig_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;
    val program : Rpc_program.t
  >>

let rec gen_to qual_id t x =
  let gen_to = gen_to qual_id in
  match t with
    | Abstract _ -> assert false

    | Var (_loc, id) -> <:expr< $lid:G.to_p id$ $x$ >>

    | Unit _loc -> <:expr< () >>
    | Int _loc -> <:expr< Rtypes.int_of_int4 (Xdr.dest_xv_int $x$) >>
    | Int32 _loc -> <:expr< Rtypes.int32_of_int4 (Xdr.dest_xv_int $x$) >>
    | Int64 _loc -> <:expr< Rtypes.int64_of_int8 (Xdr.dest_xv_hyper $x$) >>
    | Float _loc -> <:expr< Rtypes.float_of_fp8 (Xdr.dest_xv_double $x$) >>
    | Bool _loc -> <:expr< Xdr.dest_xv_enum_fast $x$ = 1 >>
    | Char _loc -> <:expr< char_of_int (Xdr.dest_xv_enum_fast $x$) >>
    | String _loc -> <:expr< Xdr.dest_xv_string $x$>>

    | Tuple (_loc, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr<
          match Xdr.dest_xv_struct_fast $x$ with
            | [| $list:pps$ |] -> ( $tup:exCom_of_list (List.map2 gen_to parts pes)$ )
            | _ -> assert false
        >>

    | Record (_loc, fields) ->
        let (fps, fes) = G.vars fields in
        let rb f e = <:rec_binding< $id:qual_id f.f_id$ = $gen_to f.f_typ e$ >> in
        <:expr<
          match Xdr.dest_xv_struct_fast $x$ with
            | [| $list:fps$ |] ->
                $ExRec(_loc, rbSem_of_list (List.map2 rb fields fes), <:expr< >>)$
            | _ -> assert false
        >>

    | Variant (_loc, arms) ->
        let mc (id, ts) i =
          match ts with
            | [] -> <:match_case< ($`int:i$, _) -> $id:qual_id id$ >>
            | [t] -> <:match_case< ($`int:i$, x) -> $id:qual_id id$ $gen_to t <:expr< x >>$ >>
            | _ ->
                let (pps, pes) = G.vars ts in
                <:match_case<
                  ($`int:i$, x) ->
                    match Xdr.dest_xv_struct_fast x with
                      | [| $list:pps$ |] ->
                          $G.apps
                            <:expr< $id:qual_id id$ >>
                            (List.map2 gen_to ts pes)$
                      | _ -> assert false
                >> in
        <:expr<
          match Xdr.dest_xv_union_over_enum_fast $x$ with
              $list:List.mapi mc arms$
            | _ -> assert false
        >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let mc (id, ts) i =
          match ts with
            | [] -> <:match_case< ($`int:i$, _) -> `$id$ >>
            | [t] -> <:match_case< ($`int:i$, x) -> `$id$ $gen_to t <:expr< x >>$ >>
            | _ ->
                let (pps, pes) = G.vars ts in
                <:match_case<
                  ($`int:i$, x) ->
                    match Xdr.dest_xv_struct_fast x with
                      | [| $list:pps$ |] ->
                          $G.apps
                            <:expr< `$id$ >>
                            (List.map2 gen_to ts pes)$
                      | _ -> assert false
                >> in
        <:expr<
          match Xdr.dest_xv_union_over_enum_fast $x$ with
              $list:List.mapi mc arms$
            | _ -> assert false
        >>

    | Array (_loc, t) ->
        <:expr< Array.map (fun x -> $gen_to t <:expr< x >>$) (Xdr.dest_xv_array $x$) >>

    | List (_loc, t) ->
        <:expr< Orpc_onc.to_list (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | Option (_loc, t) ->
        <:expr< Orpc_onc.to_option (fun x -> $gen_to t <:expr< x >>$) $x$ >>

    | Ref (_loc, t) -> <:expr< ref $gen_to t x$ >>

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

    | Unit _loc -> <:expr< Xdr.XV_void >>
    | Int _loc -> <:expr< Xdr.XV_int (Rtypes.int4_of_int $v$) >>
    | Int32 _loc -> <:expr< Xdr.XV_int (Rtypes.int4_of_int32 $v$) >>
    | Int64 _loc -> <:expr< Xdr.XV_hyper (Rtypes.int8_of_int64 $v$) >>
    | Float _loc -> <:expr< Xdr.XV_double (Rtypes.fp8_of_float $v$) >>
    | Bool _loc -> <:expr< Xdr.XV_enum_fast (if $v$ then 1 else 0) >>
    | Char _loc -> <:expr< Xdr.XV_enum_fast (int_of_char $v$) >>
    | String _loc -> <:expr< Xdr.XV_string $v$ >>

    | Tuple (_loc, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr<
          let ( $tup:paCom_of_list pps$ ) = $v$ in
          Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of parts pes)$ |] (* XXX not sure why list: doesn't work here *)
        >>

    | Record (_loc, fields) ->
        let (fps, fes) = G.vars fields in
        let rb f p = <:patt< $id:qual_id f.f_id$ = $p$ >> in
        <:expr<
          let { $paSem_of_list (List.map2 rb fields fps)$ } = $v$ in
          Xdr.XV_struct_fast
            [| $exSem_of_list (List.map2 (fun f v -> gen_of f.f_typ v) fields fes)$ |]
        >>

    | Variant (_loc, arms) ->
        let mc (id, ts) i =
          match ts with
            | [] ->
                <:match_case<
                  $id:qual_id id$ ->
                    Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void)
                >>
            | [t] ->
                <:match_case<
                  $id:qual_id id$ x ->
                    Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr< x >>$)
                >>
            | _ ->
                let (pps, pes) = G.vars ts in
                <:match_case<
                  $G.papps <:patt< $id:qual_id id$ >> pps$ ->
                    Xdr.XV_union_over_enum_fast
                      ($`int:i$,
                      Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                >> in
        <:expr< match $v$ with $list:List.mapi mc arms$ >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let mc (id, ts) i =
          match ts with
            | [] ->
                <:match_case<
                  `$id$ ->
                    Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void)
                >>
            | [t] ->
                <:match_case<
                  `$id$ x ->
                    Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr< x >>$)
                >>
            | _ ->
                let (pps, pes) = G.vars ts in
                <:match_case<
                  $G.papps <:patt< `$id$ >> pps$ ->
                    Xdr.XV_union_over_enum_fast
                      ($`int:i$,
                      Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                >> in
        <:expr< match $v$ with $list:List.mapi mc arms$ >>

    | Array (_loc, t) ->
        <:expr< Xdr.XV_array (Array.map (fun v -> $gen_of t <:expr< v >>$) $v$) >>

    | List (_loc, t) ->
        <:expr< Orpc_onc.of_list (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | Option (_loc, t) ->
        <:expr< Orpc_onc.of_option (fun v -> $gen_of t <:expr< v >>$) $v$ >>

    | Ref (_loc, t) -> gen_of t <:expr< ! $v$ >>

    | Apply (_loc, mdl, id, args) ->
        <:expr<
          $G.apps
            (<:expr< $id:G.module_id mdl (long_of_ id)$ >>)
            (List.map (fun a -> <:expr< fun v -> $gen_of a <:expr< v >>$ >>) args)$
          $v$
        >>

    | Arrow _ -> assert false

let rec exList_of_list = function
  | [] -> <:expr< [] >>
  | e :: es -> <:expr< $e$ :: $exList_of_list es$ >>

(*
  this is kind of hairy because Xdr.xdr_type_term doesn't have a nice
  way to express mutual recursion and polymorphism. the basic problem
  is that while X_rec / X_refer give you a way to tie a single
  recursive loop, for forward references in mutual recursion you have
  to inline a copy of the term you're referring to. polymorphism
  complicates things: ordinarily we express it at the OCaml level, but
  when we inline a term we have to instantiate it explicitly.

  some ways out of this (requiring Xdr modifications):

    1. provide explicit xdr_type_terms for mutually-recursive bundles
       and projection out of a bundle, and for type abstraction and
       instantiation.

    2. do everything at the OCaml level, no X_rec / X_refer. for this we
       need a way to make circular data structures but be able to recurse
       over them (see how deriving does it in Typeable).

  vs is an environment giving instantiations for type variables.
  bs is an environment listing the bound constructors (i.e. we're in the scope of an X_rec)
  ds is an environment defining constructors for forward inlining
*)

let rec gen_xdr qual_id vs bs ds t =
  let gen_xdr = gen_xdr qual_id vs bs ds in

  match t with
    | Abstract _ -> assert false

    | Var (_loc, id) ->
        begin
          try List.assoc id vs
          with Not_found -> <:expr< $lid:xdr_p id$ >>
        end

    | Unit _loc -> <:expr< Xdr.X_void >>
    | Int _loc -> <:expr< Xdr.X_int >>
    | Int32 _loc -> <:expr< Xdr.X_int >>
    | Int64 _loc -> <:expr< Xdr.X_hyper >>
    | Float _loc -> <:expr< Xdr.X_double >>
    | Bool _loc -> <:expr< Xdr.x_bool >>
    | Char _loc -> <:expr< Orpc_onc.x_char >>
    | String _loc -> <:expr< Xdr.x_string_max >>

    | Tuple (_loc, parts) ->
        let px t i = <:expr< ( $`str:string_of_int i$, $gen_xdr t$ ) >> in
        <:expr< Xdr.X_struct $exList_of_list (List.mapi px parts)$ >>

    | Record (_loc, fields) ->
        let fx f = <:expr< ( $`str:f.f_id$, $gen_xdr f.f_typ$ ) >> in
        <:expr< Xdr.X_struct $exList_of_list (List.map fx fields)$ >>

    | Variant (_loc, arms) ->
        let tag (id, _) i = <:expr< ( $`str:id$, Rtypes.int4_of_int $`int:i$ ) >> in
        let ax (id, ts) =
          match ts with
            | [] -> <:expr< ( $`str:id$, Xdr.X_void ) >>
            | [t] -> <:expr< ( $`str:id$,  $gen_xdr t$ ) >>
            | _ ->
                let px t i = <:expr< ( $`str:string_of_int i$, $gen_xdr t$ ) >> in
                <:expr< ( $`str:id$, Xdr.X_struct $exList_of_list (List.mapi px ts)$) >> in
        <:expr<
          Xdr.X_union_over_enum
            (Xdr.X_enum $exList_of_list (List.mapi tag arms)$,
            $exList_of_list (List.map ax arms)$,
            None)
        >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let tag (id, _) i = <:expr< ( $`str:id$, Rtypes.int4_of_int $`int:i$ ) >> in
        let ax (id, ts) =
          match ts with
            | [] -> <:expr< ( $`str:id$, Xdr.X_void ) >>
            | [t] -> <:expr< ( $`str:id$,  $gen_xdr t$ ) >>
            | _ ->
                let px t i = <:expr< ( $`str:string_of_int i$, $gen_xdr t$ ) >> in
                <:expr< ( $`str:id$, Xdr.X_struct $exList_of_list (List.mapi px ts)$) >> in
        <:expr<
          Xdr.X_union_over_enum
            (Xdr.X_enum $exList_of_list (List.mapi tag arms)$,
            $exList_of_list (List.map ax arms)$,
            None)
        >>

    | Array (_loc, t) -> <:expr< Xdr.x_array_max $gen_xdr t$ >>

    | List (_loc, t) -> <:expr< Orpc_onc.x_list $gen_xdr t$ >>

    | Option (_loc, t) -> <:expr< Xdr.x_optional $gen_xdr t$ >>

    | Ref (_loc, t) -> gen_xdr t

    | Apply (_, [], id, args) ->
        if List.mem id bs
        (* refer to a def in scope *)
        then <:expr< Xdr.X_refer $`str:id$ >>

        else begin
          try
            let { td_vars = vars; td_typ = t } = List.find (fun { td_id = id' } -> id' = id) ds in
            (* inline / instantiate a forward def. can just replace vs because defs have no free variables. *)
            gen_xdr_def qual_id (List.combine vars (List.map gen_xdr args)) bs ds id t

          with Not_found ->
            (* refer to a previous def at the OCaml level *)
            G.apps
              (<:expr< $lid:long_xdr id$ >>)
              (List.map gen_xdr args)
        end

    | Apply (_, mdl, id, args) ->
        G.apps
          (<:expr< $id:G.module_id mdl (long_xdr id)$ >>)
          (List.map gen_xdr args)

    | Arrow _ -> assert false

and gen_xdr_def qual_id vs bs ds id t =
  <:expr< Xdr.X_rec ($`str:id$, $gen_xdr qual_id vs (id::bs) ds t$) >>

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
          ds$ ;;

    let rec
      $list:
        List.map
          (fun { td_vars = vars; td_id = id; td_typ = t } ->
            <:binding<
              $lid:long_of_ id$ =
              $G.funs_ids
                (List.map G.of_p vars)
                <:expr< fun x -> $if stub then <:expr< assert false >> else gen_of qual_id t <:expr< x >>$ >>$
            >>)
          ds$ ;;

    $list:
      let rec loop ds =
        match ds with
          | [] -> []
          | { td_vars = vars; td_id = id; td_typ = t }::ds ->
              <:binding<
                $lid:long_xdr id$ =
                $G.funs_ids
                  (List.map xdr_p vars)
                  (if stub then <:expr< Xdr.X_void >> else gen_xdr_def qual_id [] [] ds id t)$
              >> :: loop ds in
      List.map (fun b -> <:str_item< let $b$ >>) (loop ds)$ ;;
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
      then Apply (_loc, ["Orpc_onc"], "orpc_result", [res; Apply (_loc, [], "exn", [])])
      else res in
    let items aid arg =
      <:str_item<
        let $lid:to_ aid$ x = $gen_to qual_id arg <:expr< x >>$
        let $lid:of_ aid$ v = $gen_of qual_id arg <:expr< v >>$
        let $lid:xdr aid$ = $gen_xdr qual_id [] [] [] arg$
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
         let $lid:long_to_ "exn"$ x = $gen_to qual_id t <:expr< x >>$
         let $lid:long_of_ "exn"$ v = $gen_of_exc t <:expr< v >>$
         let $lid:long_xdr "exn"$ = $gen_xdr qual_id [] [] [] t$ ;;
       >>
     else <:str_item< >>$ ;;
    $list:List.map gen_func funcs$ ;;

    let program =
      Rpc_program.create
        (Rtypes.uint4_of_int 0)
        (Rtypes.uint4_of_int 0)
        (Xdr.validate_xdr_type_system [])
        $exList_of_list
            (List.mapi
                (fun (_,id,_,_) i ->
                  <:expr<
                    $`str:id$,
                    (Rtypes.uint4_of_int $`int:i$,
                    $lid:s_arg (xdr id)$,
                    $lid:s_res (xdr id)$)
                  >>)
                funcs)$
  >>
