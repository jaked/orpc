open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let gen_srv_mli name (typedefs, excs, funcs, kinds) =

  let modules =
    List.map
      (function
        | Sync ->
            <:sig_item<
              module Sync : functor (A : $uid:name$.Sync) ->
              sig
                val bind :
                  ?program_number:Rtypes.uint4 ->
                  ?version_number:Rtypes.uint4 ->
                  Rpc_server.t -> unit
              end
            >>
        | Async ->
            <:sig_item<
              module Async : functor (A : $uid:name$.Async) ->
              sig
                val bind :
                  ?program_number:Rtypes.uint4 ->
                  ?version_number:Rtypes.uint4 ->
                  Rpc_server.t -> unit
              end
            >>
        | Lwt ->
            <:sig_item<
              module Lwt : functor (A : $uid:name$.Lwt) ->
              sig
                val bind :
                  ?program_number:Rtypes.uint4 ->
                  ?version_number:Rtypes.uint4 ->
                  Rpc_server.t -> unit
              end
            >>)
      kinds in

  <:sig_item<
    val bind :
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      $List.fold_right
        (fun (_, id, args, _) t ->
          <:ctyp<
            $lid:"proc_" ^ id$ :
            $G.arrows
              (List.mapi
                  (fun a i -> G.labelled_ctyp a (G.aux_type name (G.argi id i)))
                  args)
              (G.aux_type name (G.res0 id))$
            -> $t$
          >>)
        funcs
        <:ctyp< Rpc_server.t -> unit >>$ ;;

    val bind_async :
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      $List.fold_right
        (fun (_, id, args, _) t ->
          <:ctyp<
            $lid:"proc_" ^ id$ :
            $G.arrows
              (<:ctyp< Rpc_server.session >> ::
                  (List.mapi
                      (fun a i -> G.labelled_ctyp a (G.aux_type name (G.argi id i)))
                      args))
              <:ctyp< ($G.aux_type name (G.res0 id)$ -> unit) -> unit >>$
            -> $t$
          >>)
        funcs
        <:ctyp< Rpc_server.t -> unit >>$ ;;

    $sgSem_of_list modules$
  >>



let gen_srv_ml name (typedefs, excs, funcs, kinds) =

  let sync_func ~has_excs (_, id, args, _) =
    <:expr<
      Rpc_server.Sync {
        Rpc_server.sync_name = $`str:id$;
        Rpc_server.sync_proc = fun x ->
          $G.aux_val name (G.of_res id)$
          $(fun body ->
              if has_excs
              then <:expr< Orpc.pack_orpc_result (fun () -> $body$) >>
              else body)
            (match args with
              | [] -> assert false
              | [a] ->
                  <:expr<
                    ($lid:"proc_" ^ id$ ($G.labelled_expr a (G.aux_val name (G.to_arg id))$ x))
                  >>
              | _ ->
                  let (ps, es) = G.vars args in
                  <:expr<
                    let ( $paCom_of_list ps$ ) = $G.aux_val name (G.to_arg id)$ x in
                    $G.apps
                      <:expr< $lid:"proc_" ^ id$ >>
                      (List.map2 G.labelled_expr args es)$
                  >>)$
      }
    >> in

  let async_func ~has_excs (_, id, args, _) =
    <:expr<
      Rpc_server.Async {
        Rpc_server.async_name = $`str:id$;
        Rpc_server.async_invoke = fun s x ->
          $(fun body body2 ->
              if has_excs
              then <:expr< Orpc.pack_orpc_result_async (fun k -> $body$ k) $body2$ >>
              else <:expr< $body$ $body2$ >>)
            (match args with
              | [] -> assert false
              | [a] ->
                  <:expr<
                    $lid:"proc_" ^ id$ s ($G.labelled_expr a (G.aux_val name (G.to_arg id))$ x)
                  >>
              | _ ->
                  let (ps, es) = G.vars args in
                  <:expr<
                    let ( $paCom_of_list ps$ ) = $G.aux_val name (G.to_arg id)$ x in
                    $G.apps
                      <:expr< $lid:"proc_" ^ id$ s >>
                      (List.map2 G.labelled_expr args es)$
                  >>)
            <:expr< (fun y -> Rpc_server.reply s ($G.aux_val name (G.of_res id)$ y)) >>$
        }
      >> in

  let modules =
    List.map
      (function
        | Sync ->
            <:str_item<
              module Sync (A : $uid:name$.Sync) =
              struct
                let bind
                    ?program_number
                    ?version_number
                    srv =
                  $List.fold_left
                    (fun e (_, id, _, _) ->
                      (* <:expr< $e$ ~ $lid:"proc" ^ id$ : A.$lid:id$ >> does not work? *)
                      ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, <:expr< A.$lid:id$ >>)))
                    <:expr< bind ?program_number ?version_number >>
                    funcs$
                  srv
              end
            >>

        | Async ->
            <:str_item<
              module Async (A : $uid:name$.Async) =
              struct
                let bind
                    ?program_number
                    ?version_number
                    srv =
                  $List.fold_left
                    (fun e (_, id, args, _) ->
                      let body =
                        <:expr<
                          fun s ->
                            $G.args_funs args
                              <:expr<
                                fun pass_reply ->
                                  Orpc.session := Some s;
                                  $G.args_apps <:expr< A.$lid:id$ >> args$
                                    (fun r -> pass_reply (r ()))
                              >>$
                        >> in
                      ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, body)))
                    <:expr< bind_async ?program_number ?version_number >>
                    funcs$
                  srv
              end
            >>

        | Lwt ->
            <:str_item<
              module Lwt (A : $uid:name$.Lwt) =
              struct
                let bind
                    ?program_number
                    ?version_number
                    srv =
                  $List.fold_left
                    (fun e (_, id, args, _) ->
                      let body =
                        <:expr<
                          fun s ->
                            $G.args_funs args
                               <:expr<
                                 fun pass_reply ->
                                   Orpc.session := Some s;
                                   Lwt.ignore_result
                                     (Lwt.try_bind
                                         (fun () ->
                                           $G.args_apps <:expr< A.$lid:id$ >> args$)
                                         (fun r -> Lwt.return (pass_reply r))
                                         (fun e -> raise e))
                               >>$
                        >> in
                      ExApp(_loc, e, ExLab (_loc, "proc_" ^ id, body)))
                    <:expr< bind_async ?program_number ?version_number >>
                    funcs$
                  srv
              end
            >>)
      kinds in

  let has_excs = excs <> [] in
  <:str_item<
    let bind
        ?program_number
        ?version_number =
      $List.fold_right
        (fun (_, id, _, _) e -> <:expr< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
        funcs
        <:expr<
          fun srv ->
            Rpc_server.bind
              ?program_number ?version_number $G.aux_val name "program"$
              $G.conses (List.map (sync_func ~has_excs) funcs)$
              srv
        >>$ ;;

    let bind_async
        ?program_number
        ?version_number =
      $List.fold_right
        (fun (_, id, _, _) e -> <:expr< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
        funcs
        <:expr<
          fun srv ->
            Rpc_server.bind
              ?program_number ?version_number $G.aux_val name "program"$
              $G.conses (List.map (async_func ~has_excs) funcs)$
              srv
        >>$ ;;

    $stSem_of_list modules$
  >>
