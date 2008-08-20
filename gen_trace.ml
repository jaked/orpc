open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let format_ id = "format_" ^ id
let format_p id = "fmt'" ^ id

let typ_of_argtyp_option = function
  | Unlabelled (_, t) -> t
  | Labelled (_, _, t) -> t
  | Optional (loc, _, t) -> Option (loc, t)

let gen_trace_mli name (typedefs, excs, funcs, kinds) =

  let type_mod =
    match kinds with
      | [] -> name ^ "_aux"
      | _ -> name in

  let modules =
    List.map
      (fun kind ->
        let mt =
          match kind with
            | Sync -> "Sync"
            | Async -> "Async"
            | Lwt -> "Lwt" in
        <:sig_item@g<
          module $uid:mt$ :
            functor (T : Orpc.Trace) ->
              functor (A : $uid:name$.$uid:mt$) ->
                $uid:name$.$uid:mt$
        >>)
      kinds in

  let gen_typedef ds =
    let is =
      List.map
        (fun (_, vars, id, _) ->
          let appd =
            G.tapps <:ctyp< $uid:type_mod$ . $lid:id$ >> (List.map (fun v -> <:ctyp< '$lid:v$ >>) vars) in

          <:sig_item<
            val $lid:format_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp< Format.formatter -> '$lid:v$ -> unit >>) vars)
                <:ctyp< Format.formatter -> $appd$ -> unit >>$
          >>)
        ds in
    sgSem_of_list is in

  <:sig_item<
    $sgSem_of_list (List.map gen_typedef typedefs)$ ;;

    val format_exn : Format.formatter -> exn -> unit;;

    $sgSem_of_list modules$
  >>

let gen_trace_ml name (typedefs, excs, funcs, kinds) =

  let type_mod =
    match kinds with
      | [] -> name ^ "_aux"
      | _ -> name in

  let rec gen_format t =
    match t with
      | Var (_, id) -> <:expr< $lid:format_p id$ >>
      | Unit _ -> <:expr< fun fmt _ -> Format.fprintf fmt "()" >>
      | Int _ -> <:expr< fun fmt v -> Format.fprintf fmt "%d" v >>
      | Int32 _ -> <:expr< fun fmt v -> Format.fprintf fmt "%ld" v >>
      | Int64 _ -> <:expr< fun fmt v -> Format.fprintf fmt "%Ld" v >>
      | Float _ -> <:expr< fun fmt v -> Format.fprintf fmt "%g" v >>
      | Bool _ -> <:expr< fun fmt v -> Format.fprintf fmt "%B" v >>
      | Char _ -> <:expr< fun fmt v -> Format.fprintf fmt "%C" v >>
      | String _ -> <:expr< fun fmt v -> Format.fprintf fmt "%S" v >>

      | Tuple (_, parts) ->
          let (pps, pes) = G.vars parts in
          let spec =
            String.concat "" [
              "@[<hv 1>(";
              String.concat ",@ " (List.map (fun _ -> "%a") parts);
              ")@]";
            ] in
          <:expr<
            fun fmt v ->
              let ( $paCom_of_list pps$ ) = v in
              $G.apps
                <:expr< Format.fprintf fmt $`str:spec$ >>
                (List.fold_right2
                    (fun t v l -> (gen_format t)::v::l)
                    parts pes [])$
          >>

      | Record (_, fields) ->
          let (fps, fes) = G.vars fields in
          let spec =
            String.concat "" [
              "@[{@[<hv 2>@ ";
              String.concat ";@ "
                (List.map (fun f -> "@[<hov 2>" ^ f.f_id ^ "@ =@ %a@]") fields);
              "@]@ }@]";
            ] in
          let rb f p = <:patt< $Ast.IdAcc(_loc, Ast.IdUid (_loc, type_mod), Ast.IdLid (_loc, f.f_id))$ = $p$ >> in
          <:expr<
            fun fmt v ->
              let { $paSem_of_list (List.map2 rb fields fps)$ } = v in
              $G.apps
                <:expr< Format.fprintf fmt $`str:spec$ >>
                (List.fold_right2
                    (fun f v l -> (gen_format f.f_typ)::v::l)
                    fields fes [])$
          >>

      | Variant (_, arms) ->
          let mc (id, ts) =
            match ts with
              | [] ->
                  <:match_case<
                    $uid:type_mod$.$uid:id$ ->
                      Format.fprintf fmt $`str:id$;
                  >>
              | [t] ->
                  let spec = String.concat "" [ "@[<hv 1>("; id; "@ %a)@]"; ] in
                  <:match_case<
                    $uid:type_mod$.$uid:id$ x ->
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
                    $uid:type_mod$.$uid:id$ ( $paCom_of_list pps$ ) ->
                      $G.apps
                        <:expr< Format.fprintf fmt $`str:spec$ >>
                        (List.fold_right2
                            (fun t v l -> (gen_format t)::v::l)
                            ts pes [])$
                  >> in
          <:expr<
            fun fmt v -> $ExMat (_loc, <:expr< v >>, mcOr_of_list (List.map mc arms))$
          >>

      | Array (_, t) -> <:expr< Orpc.format_array $gen_format t$ >>

      | List (_, t) -> <:expr< Orpc.format_list $gen_format t$ >>

      | Option (_, t) -> <:expr< Orpc.format_option $gen_format t$ >>

      | Apply (_, mdl, id, args) ->
          G.apps
            (match mdl with
              | None -> <:expr< $lid:format_ id$ >>
              | Some mdl -> <:expr< $uid:mdl$ . $lid:format_ id$ >>)
            (List.map gen_format args)

     | Arrow _ -> assert false in

  let gen_format_exc t =
    match gen_format t with
      | <:expr< fun fmt v -> $ExMat (loc, e, cases)$ >> ->
          <:expr<
            fun fmt v ->
              $ExMat (loc, e, McOr(g, cases, <:match_case@g< _ -> Format.fprintf fmt "<exn>" >>))$
          >>
      | _ -> assert false in

  let gen_typedef ds =
    <:str_item<
      $let es =
        List.map
          (fun (_, vars, id, t) ->
            <:binding<
              $lid:format_ id$ =
              $G.funs_ids
                (List.map format_p vars)
                (gen_format t)$
            >>)
          ds in
      StVal (_loc, BTrue, biAnd_of_list es)$;;
    >> in

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
      let $lid:format_ id ^ "'call"$ fmt =
        $G.args_funs args
          <:expr<
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun t v l -> (gen_format (typ_of_argtyp_option t))::v::l)
                  args es [])$
          >>$
      let $lid:format_ id ^ "'reply"$ fmt res =
        Format.fprintf fmt "@[<hv 2><==@ %a@]@."
          $gen_format res$
          res
    >> in

  let gen_module kind =
    let mt =
      match kind with
        | Sync -> "Sync"
        | Async -> "Async"
        | Lwt -> "Lwt" in

    let func (_, id, args, res) =

      let trace_call =
        <:expr< T.trace_call $`str:id$ (fun fmt -> $G.args_apps <:expr< $lid:format_ id ^ "'call"$ fmt >> args$) >> in
      let trace_reply_ok =
        <:expr< T.trace_reply_ok t (fun fmt -> $lid:format_ id ^ "'reply"$ fmt r) >> in
      let trace_reply_exn =
        <:expr< T.trace_reply_exn t e (fun fmt -> format_exn'reply fmt e) >> in

      <:str_item<
        let $lid:id$ =
          $G.args_funs args
            (match kind with
              | Sync ->
                  <:expr<
                    let t = $trace_call$ in
                    try let r = $G.args_apps <:expr< A.$lid:id$ >> args$ in $trace_reply_ok$; r
                    with e -> $trace_reply_exn$; raise e
                  >>

              | Async ->
                  <:expr<
                    fun pass_reply ->
                      let t = $trace_call$ in
                      let pass_reply rf =
                        (try let r = rf () in $trace_reply_ok$
                          with e -> $trace_reply_exn$);
                        pass_reply rf in
                      $G.args_apps <:expr< A.$lid:id$ >> args$ pass_reply
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

    <:str_item<
      module $uid:mt$
        (T : Orpc.Trace)
        (A : $uid:name$.$uid:mt$) =
      struct
        $stSem_of_list (List.map func funcs)$
      end
    >> in

  <:str_item<
    let format_exn =
      $gen_format_exc
        (Variant (g, List.map (fun (_, id, ts) -> (id, ts)) excs))$ ;;

    let format_exn'reply fmt exn =
      Format.fprintf fmt "@[<hv 2><=!@ %a@]@." format_exn exn ;;

    $stSem_of_list (List.map gen_typedef typedefs)$;;

    $stSem_of_list (List.map gen_func funcs)$;;

    $stSem_of_list (List.map gen_module kinds)$
  >>
