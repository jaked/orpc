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

let pp_ id = "orpc_trace_pp_" ^ id
let pp_p id = "pp'" ^ id

let gen_sig_typedef ?(qual_id=G.id) ds =
  let is =
    List.map
      (fun { td_vars = vars; td_id = id } ->
        let appd =
          G.tapps <:ctyp< $id:qual_id id$ >> (List.map (fun v -> <:ctyp< '$lid:v$ >>) vars) in

        <:sig_item<
       val $lid:pp_ id$ :
         $G.arrows
         (List.map (fun v -> <:ctyp< Format.formatter -> '$lid:v$ -> unit >>) vars)
         <:ctyp< Format.formatter -> $appd$ -> unit >>$
         >>)
      ds in
  sgSem_of_list is

let gen_module_type name (typedefs, _, funcs, kinds) =

  let qual_id = G.qual_id_aux name in

  let gen_func  (_, id, args, res) =
    <:sig_item<
      val $lid:pp_ id ^ "'call"$ : Format.formatter -> $G.args_arrows qual_id args <:ctyp< unit >>$ ;;
      val $lid:pp_ id ^ "'reply"$ : Format.formatter -> $G.gen_type qual_id res$ -> unit ;;
    >> in

  <:sig_item<
    $list:List.map (gen_sig_typedef ~qual_id) typedefs$ ;;
    $list:List.map gen_func funcs$ ;;
    val $lid:pp_ "exn"$ : Format.formatter -> exn -> unit;;
    val $lid:pp_ "exn'reply"$ : Format.formatter -> exn -> unit;;
  >>

let gen_mli name (typedefs, excs, funcs, kinds) =

  let modules =
    List.map
      (fun kind ->
        let mt = G.string_of_kind kind in
        <:sig_item<
          module $uid:mt ^ "_pp"$ (P : Pp) (T : Orpc_pp.Trace) (A : $uid:name$.$uid:mt$) : $uid:name$.$uid:mt$
          module $uid:mt$ (T : Orpc_pp.Trace) (A : $uid:name$.$uid:mt$) : $uid:name$.$uid:mt$
        >>)
      kinds in

  <:sig_item<
    module type Pp =
    sig
      $gen_module_type name (typedefs, excs, funcs, kinds)$
    end

    module Pp_pp (P : Pp) : Pp ;;
    module Pp : Pp ;;

    $list:modules$
  >>

let rec gen_format qual_id rec_mod t =
  let gen_format = gen_format qual_id rec_mod in
  match t with
    | Abstract _ -> assert false

    | Var (_loc, id) -> <:expr< $lid:pp_p id$ >>

    | Unit _loc -> <:expr< fun fmt _ -> Format.fprintf fmt "()" >>
    | Int _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%d" v >>
    | Int32 _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%ld" v >>
    | Int64 _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%Ld" v >>
    | Float _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%g" v >>
    | Bool _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%B" v >>
    | Char _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%C" v >>
    | String _loc -> <:expr< fun fmt v -> Format.fprintf fmt "%S" v >>

    | Tuple (_loc, parts) ->
        let (pps, pes) = G.vars parts in
        let spec =
          String.concat "" [
            "@[<hv 1>(";
            String.concat ",@ " (List.map (fun _ -> "%a") parts);
            ")@]";
          ] in
        <:expr<
          fun fmt v ->
            let ( $tup:paCom_of_list pps$ ) = v in
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun t v l -> (gen_format t)::v::l)
                  parts pes [])$
        >>

    | Record (_loc, fields) ->
        let (fps, fes) = G.vars fields in
        let spec =
          String.concat "" [
            "@[{@[<hv 2>@ ";
            String.concat ";@ "
              (List.map (fun f -> "@[<hov 2>" ^ f.f_id ^ "@ =@ %a@]") fields);
            "@]@ }@]";
          ] in
        let rb f p = <:patt< $id:qual_id f.f_id$ = $p$ >> in
        <:expr<
          fun fmt v ->
            let { $paSem_of_list (List.map2 rb fields fps)$ } = v in
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun f v l -> (gen_format f.f_typ)::v::l)
                  fields fes [])$
        >>

    | Variant (_loc, arms) ->
        let mc (id, ts) =
          match ts with
            | [] ->
                <:match_case<
                  $id:qual_id id$ -> Format.fprintf fmt $`str:id$;
                >>
            | [t] ->
                let spec = String.concat "" [ "@[<hv 1>("; id; "@ %a)@]"; ] in
                <:match_case<
                  $id:qual_id id$ x ->
                    Format.fprintf fmt $`str:spec$
                      $gen_format t$
                      x
                >>
            | _ ->
                let (pps, pes) = G.vars ts in
                let spec =
                  String.concat "" [
                    "@[<hv 1>(";
                    id;
                    "@ @[<hv 1>(";
                    String.concat ",@ " (List.map (fun _ -> "%a") ts);
                    ")@])@]";
                  ] in
                <:match_case<
                  $G.papps <:patt< $id:qual_id id$>> pps$ ->
                    $G.apps
                      <:expr< Format.fprintf fmt $`str:spec$ >>
                      (List.fold_right2
                          (fun t v l -> (gen_format t)::v::l)
                          ts pes [])$
                >> in
        <:expr< fun fmt v -> match v with $list:List.map mc arms$ >>

    | PolyVar (_loc, _, arms) ->
        let arms = List.map (function Pv_pv _ -> assert false | Pv_of (id, ts) -> (id, ts)) arms in
        let mc (id, ts) =
          match ts with
            | [] ->
                <:match_case<
                  `$id$ -> Format.fprintf fmt $`str:"`"^id$;
                >>
            | [t] ->
                let spec = String.concat "" [ "@[<hv 1>(`"; id; "@ %a)@]"; ] in
                <:match_case<
                  `$id$ x ->
                    Format.fprintf fmt $`str:spec$
                      $gen_format t$
                      x
                >>
            | _ ->
                let (pps, pes) = G.vars ts in
                let spec =
                  String.concat "" [
                    "@[<hv 1>(`";
                    id;
                    "@ @[<hv 1>(";
                    String.concat ",@ " (List.map (fun _ -> "%a") ts);
                    ")@])@]";
                  ] in
                <:match_case<
                  $G.papps <:patt< `$id$>> pps$ ->
                    $G.apps
                      <:expr< Format.fprintf fmt $`str:spec$ >>
                      (List.fold_right2
                          (fun t v l -> (gen_format t)::v::l)
                          ts pes [])$
                >> in
        <:expr< fun fmt v -> match v with $list:List.map mc arms$ >>

    | Array (_loc, t) -> <:expr< Orpc_pp.pp_array $gen_format t$ >>

    | List (_loc, t) -> <:expr< Orpc_pp.pp_list $gen_format t$ >>

    | Option (_loc, t) -> <:expr< Orpc_pp.pp_option $gen_format t$ >>

    | Ref (_loc, t) -> <:expr< fun fmt v -> Format.fprintf fmt "@[<hv 1>(ref@ %a)@]" $gen_format t$ v >>

    | Apply (_loc, mdl, id, args) ->
        G.apps
          (match mdl, rec_mod with
            | [], true -> <:expr< P.$lid:pp_ id$ >>
            | [], false -> <:expr< $lid:pp_ id$ >>
            | _ -> <:expr< $id:G.module_id mdl (pp_ id)$ >>)
          (List.map gen_format args)

   | Arrow _ -> assert false

let gen_str_typedef ?(qual_id=G.id) ?(rec_mod=true) stub ds =
  let es =
    List.map
      (fun { td_vars = vars; td_id = id; td_typ = t } ->
        <:binding<
          $lid:pp_ id$ =
          $G.funs_ids
            (List.map pp_p vars)
            (if stub then <:expr< assert false >> else gen_format qual_id rec_mod t)$
          >>)
      ds in
  <:str_item< let rec $list:es$ >>

let gen_ml name (typedefs, excs, funcs, kinds) =

  let qual_id = G.qual_id_aux name in

  let gen_pp_exc t =
    match gen_format qual_id true t with
      | <:expr< fun fmt v -> $ExMat (loc, e, cases)$ >> ->
          <:expr<
            fun fmt v ->
              match $e$ with
                  $cases$
                | _ -> Format.fprintf fmt "<exn>"
          >>
      | _ -> assert false in

  let gen_func (_, id, args, res) =
    let (_, es) = G.vars args in
    let spec =
      String.concat "" [
        "@[<hv 2>";
        id;
        "@ ";
        String.concat "@ "
          (List.map
              (function
                | Unlabelled _ -> "%a"
                | Labelled (_, label, _) -> "~" ^ label ^ ":%a"
                | Optional (_, label, _) -> "?" ^ label ^ ":%a")
              args);
        "@]@."
      ] in
    <:str_item<
      let $lid:pp_ id ^ "'call"$ fmt =
        $G.args_funs args
          <:expr<
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun t v l -> (gen_format qual_id true (typ_of_argtyp_option t))::v::l)
                  args es [])$
          >>$
      let $lid:pp_ id ^ "'reply"$ fmt res =
        Format.fprintf fmt "@[<hv 2><==@ %a@]@."
          $gen_format qual_id true res$
          res
    >> in

  let gen_module kind =
    let func (_, id, args, res) =

      let trace_call =
        <:expr< T.trace_call $`str:id$ (fun fmt -> $G.args_apps <:expr< P.$lid:pp_ id ^ "'call"$ fmt >> args$) >> in
      let trace_reply_ok =
        <:expr< T.trace_reply_ok t (fun fmt -> P.$lid:pp_ id ^ "'reply"$ fmt r) >> in
      let trace_reply_exn =
        <:expr< T.trace_reply_exn t e (fun fmt -> P.$lid:pp_ "exn'reply"$ fmt e) >> in

      <:str_item<
        let $lid:id$ =
          $G.args_funs args
            (match kind with
              | Ik_abstract -> assert false

              | Sync ->
                  <:expr<
                    let t = $trace_call$ in
                    try let r = $G.args_apps <:expr< A.$lid:id$ >> args$ in $trace_reply_ok$; r
                    with e -> $trace_reply_exn$; raise e
                  >>

              | Lwt ->
                  <:expr<
                    let t = $trace_call$ in
                    Lwt.try_bind
                      (fun () -> $G.args_apps <:expr< A.$lid:id$ >> args$)
                      (fun r -> $trace_reply_ok$; Lwt.return r)
                      (fun e -> $trace_reply_exn$; Lwt.fail e)
                  >>)$
      >> in

    let mt = G.string_of_kind kind in
    <:str_item<
      module $uid:mt ^ "_pp"$
        (P : Pp)
        (T : Orpc_pp.Trace)
        (A : $uid:name$.$uid:mt$) =
      struct
        $G._r_of_kind kind$;;
        $list:List.map func funcs$
      end

      module $uid:mt$ = $uid:mt ^ "_pp"$ (Pp)
    >> in

  let modules = List.map gen_module kinds in

  <:str_item<
    module type Pp =
    sig
      $gen_module_type name (typedefs, excs, funcs, kinds)$
    end

    module Pp_pp (P : Pp) : Pp =
    struct
      let $lid:pp_ "exn"$ =
        $gen_pp_exc
          (Variant (g, List.map (fun (_, id, ts) -> (id, ts)) excs))$ ;;

      let $lid:pp_ "exn'reply"$ fmt exn =
        Format.fprintf fmt "@[<hv 2><=!@ %a@]@." P.$lid:pp_ "exn"$ exn ;;

      $list:List.map (gen_str_typedef ~qual_id false) typedefs$;;

      $list:List.map gen_func funcs$;;
    end

    module rec Pp : Pp =
    struct
      include Pp_pp(Pp)
    end

    $list:modules$
  >>
