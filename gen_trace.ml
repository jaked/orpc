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
      (function
        | Sync ->
            <:sig_item@g<
              module Sync
                (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
                (A : $uid:name$.Sync) : $uid:name$.Sync
            >>
        | Async ->
            <:sig_item@g<
              module Async
                (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
                (A : $uid:name$.Async) : $uid:name$.Async
            >>
        | Lwt ->
            <:sig_item@g<
              module Lwt
                (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
                (A : $uid:name$.Lwt) : $uid:name$.Lwt
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

  let rec gen_format t v =
    match t with
      | Var (_, id) -> <:expr< $lid:format_p id$ fmt $v$ >>
      | Unit _ -> <:expr< Format.fprintf fmt "()" >>
      | Int _ -> <:expr< Format.fprintf fmt "%d" $v$ >>
      | Int32 _ -> <:expr< Format.fprintf fmt "%ld" $v$ >>
      | Int64 _ -> <:expr< Format.fprintf fmt "%Ld" $v$ >>
      | Float _ -> <:expr< Format.fprintf fmt "%g" $v$ >>
      | Bool _ -> <:expr< Format.fprintf fmt "%B" $v$ >>
      | Char _ -> <:expr< Format.fprintf fmt "%C" $v$ >>
      | String _ -> <:expr< Format.fprintf fmt "%S" $v$ >>

      | Tuple (_, parts) ->
          let (pps, pes) = G.vars parts in
          let spec =
            String.concat "" [
              "@[<hv 1>(";
              String.concat ",@ " (List.map (fun _ -> "%a") parts);
              ")@]";
            ] in
          <:expr<
            let ( $paCom_of_list pps$ ) = $v$ in
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun t v l -> (gen_format_fun t)::v::l)
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
            let { $paSem_of_list (List.map2 rb fields fps)$ } = $v$ in
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun f v l -> (gen_format_fun f.f_typ)::v::l)
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
                        $gen_format_fun t$
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
                            (fun t v l -> (gen_format_fun t)::v::l)
                            ts pes [])$
                  >> in
          ExMat (_loc, v, mcOr_of_list (List.map mc arms))

      | Array (_, t) -> <:expr< Orpc.format_array $gen_format_fun t$ fmt $v$ >>

      | List (_, t) -> <:expr< Orpc.format_list $gen_format_fun t$ fmt $v$ >>

      | Option (_, t) -> <:expr< Orpc.format_option $gen_format_fun t$ fmt $v$ >>

      | Apply (_, mdl, id, args) ->
          <:expr<
            $G.apps
              (match mdl with
                | None -> <:expr< $lid:format_ id$ >>
                | Some mdl -> <:expr< $uid:mdl$ . $lid:format_ id$ >>)
              (List.map gen_format_fun args)$
            fmt $v$
          >>

     | Arrow _ -> assert false

  and gen_format_fun t =
    (* XXX don't repeat yourself *)
    match t with
      | Var (_, id) -> <:expr< $lid:format_p id$ >>
      | Array (_, t) -> <:expr< Orpc.format_array $gen_format_fun t$ >>
      | List (_, t) -> <:expr< Orpc.format_list $gen_format_fun t$ >>
      | Option (_, t) -> <:expr< Orpc.format_option $gen_format_fun t$ >>
      | Apply (_, mdl, id, args) ->
          G.apps
            (match mdl with
              | None -> <:expr< $lid:format_ id$ >>
              | Some mdl -> <:expr< $uid:mdl$ . $lid:format_ id$ >>)
            (List.map gen_format_fun args)
      | _ -> <:expr< fun fmt v -> $gen_format t <:expr< v >>$ >> in

  let gen_format_fun_exc t =
    match gen_format t <:expr< v >> with
      | ExMat (loc, e, cases) ->
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
                (gen_format_fun t)$
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
      let $lid:format_ id ^ "'args"$ fmt =
        $G.args_funs args
          <:expr<
            $G.apps
              <:expr< Format.fprintf fmt $`str:spec$ >>
              (List.fold_right2
                  (fun t v l -> (gen_format_fun (typ_of_argtyp_option t))::v::l)
                  args es [])$
          >>$
      let $lid:format_ id ^ "'res"$ fmt res =
        Format.fprintf fmt "@[<hv 2><==@ %a@]@."
          $gen_format_fun res$
          res
    >> in

  let modules =
    ListLabels.map kinds ~f:(function

      | Sync ->
          let func (_, id, args, res) =
            <:str_item<
              let $lid:id$ =
                $G.args_funs args
                  <:expr<
                    (try
                        T.with_formatter (fun fmt ->
                          $G.args_apps <:expr< $lid:format_ id ^ "'args"$ fmt >> args$);
                      with _ -> ());
                    try
                      let r = $G.args_apps <:expr< A.$lid:id$ >> args$ in
                      (try T.with_formatter (fun fmt -> $lid:format_ id ^ "'res"$ fmt r) with _ -> ());
                      r
                    with e ->
                      (try T.with_formatter (fun fmt -> format_exn_res fmt e) with _ -> ());
                      raise e
                  >>$
            >> in
          <:str_item<
            module Sync
              (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
              (A : $uid:name$.Sync) =
            struct
              $stSem_of_list (List.map func funcs)$
            end
          >>

      | Async ->
          let func (_, id, args, res) =
            <:str_item<
              let $lid:id$ =
                $G.args_funs args
                  <:expr<
                    fun pass_reply ->
                      (try
                          T.with_formatter (fun fmt ->
                            $G.args_apps <:expr< $lid:format_ id ^ "'args"$ fmt >> args$);
                        with _ -> ());
                      let pass_reply rf =
                        (try
                            let r = rf () in
                            (try T.with_formatter (fun fmt -> $lid:format_ id ^ "'res"$ fmt r) with _ -> ())
                          with e ->
                            (try T.with_formatter (fun fmt -> format_exn_res fmt e) with _ -> ()));
                        pass_reply rf in
                      $G.args_apps <:expr< A.$lid:id$ >> args$ pass_reply
                  >>$
            >> in
          <:str_item<
            module Async
              (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
              (A : $uid:name$.Async) =
            struct
              $stSem_of_list (List.map func funcs)$
            end
          >>

      | Lwt ->
          let func (_, id, args, res) =
            <:str_item<
              let $lid:id$ =
                $G.args_funs args
                  <:expr<
                    (try
                        T.with_formatter (fun fmt ->
                          $G.args_apps <:expr< $lid:format_ id ^ "'args"$ fmt >> args$);
                      with _ -> ());
                    Lwt.try_bind
                      (fun () -> $G.args_apps <:expr< A.$lid:id$ >> args$)
                      (fun r ->
                        (try T.with_formatter (fun fmt -> $lid:format_ id ^ "'res"$ fmt r) with _ -> ());
                        Lwt.return r)
                      (fun e ->
                        (try T.with_formatter (fun fmt -> format_exn_res fmt e) with _ -> ());
                        Lwt.fail e)
                  >>$
            >> in
          <:str_item<
            module Lwt
              (T : sig val with_formatter : (Format.formatter -> unit) -> unit end)
              (A : $uid:name$.Lwt) =
            struct
              $stSem_of_list (List.map func funcs)$
            end
          >>) in

  <:str_item<
    let format_exn =
      $gen_format_fun_exc
        (Variant (g, List.map (fun (_, id, ts) -> (id, ts)) excs))$ ;;

    let format_exn_res fmt exn =
      Format.fprintf fmt "@[<hv 2><=!@ %a@]@." format_exn exn ;;

    $stSem_of_list (List.map gen_typedef typedefs)$;;

    $stSem_of_list (List.map gen_func funcs)$;;

    $stSem_of_list modules$
  >>
