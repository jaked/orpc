open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let g = Camlp4.PreCast.Loc.ghost

let gen_srv_mli name intf =
  let modules =
    match intf with
      | Simple _ -> <:sig_item@g< >>

      | Modules (_, (_, sync_name, _), _) ->
          <:sig_item@g<
            module Sync : functor (A : $uid:name$.$uid:sync_name$) ->
            sig
              val bind :
                ?program_number:Rtypes.uint4 ->
                ?version_number:Rtypes.uint4 ->
                Rpc_server.t -> unit
            end
          >> in

  match intf with
    | Simple (_, funcs)
    | Modules (_, (_, _, funcs), _) ->
        <:sig_item@g<
          val bind :
            ?program_number:Rtypes.uint4 ->
            ?version_number:Rtypes.uint4 ->
            $List.fold_right
              (fun (_, id, args, _) t ->
                <:ctyp@g<
                  $lid:"proc_" ^ id$ :
                  $G.arrows
                    (List.mapi (fun _ i -> G.aux_type name (G.argi id i)) args)
                    (G.aux_type name (G.res id))$
                  -> $t$
                >>)
              funcs
              <:ctyp@g< Rpc_server.t -> unit >>$ ;;

          $modules$

          val bind_async :
            ?program_number:Rtypes.uint4 ->
            ?version_number:Rtypes.uint4 ->
            $List.fold_right
              (fun (_, id, args, _) t ->
                <:ctyp@g<
                  $lid:"proc_" ^ id$ :
                  $G.arrows
                    (<:ctyp@g< Rpc_server.session >> ::
                        List.mapi (fun _ i -> G.aux_type name (G.argi id i)) args)
                    <:ctyp@g< ($G.aux_type name (G.res id)$ -> unit) -> unit >>$
                  -> $t$
                >>)
              funcs
              <:ctyp@g< Rpc_server.t -> unit >>$ ;;
        >>



let gen_srv_ml name intf =

  let sync_func (_, id, args, _) =
    <:expr@g<
      Rpc_server.Sync {
        Rpc_server.sync_name = $`str:id$;
        Rpc_server.sync_proc =
          $match args with
            | [] -> assert false
            | [_] ->
                <:expr@g<
                  fun x ->
                    $G.aux_val name (G.of_res id)$
                      ($lid:"proc_" ^ id$
                          ($G.aux_val name (G.to_arg id)$ x))
                >>
            | _ ->
                let (ps, es) = G.vars args in
                <:expr@g<
                  fun x ->
                    let ( $paCom_of_list ps$ ) = $G.aux_val name (G.to_arg id)$ x in
                    $G.aux_val name (G.of_res id)$
                      $G.apps <:expr@g< $lid:"proc_" ^ id$ >> es$
                >>$
      }
    >> in

  let async_func (_, id, args, _) =
    <:expr@g<
      Rpc_server.Async {
        Rpc_server.async_name = $`str:id$;
        Rpc_server.async_invoke =
          $match args with
            | [] -> assert false
            | [_] ->
                <:expr@g<
                  fun s x ->
                    $lid:"proc_" ^ id$
                      s ($G.aux_val name (G.to_arg id)$ x)
                      (fun y -> Rpc_server.reply s ($G.aux_val name (G.of_res id)$ y))
                >>
            | _ ->
                let (ps, es) = G.vars args in
                <:expr@g<
                  fun s x ->
                    let ( $paCom_of_list ps$ ) = $G.aux_val name (G.to_arg id)$ x in
                    $G.apps <:expr@g< $lid:"proc_" ^ id$ >> (<:expr@g< s >>::es)$
                      (fun y -> Rpc_server.reply s ($G.aux_val name (G.of_res id)$ y))
                >>$
      }
    >> in

  let modules =
    match intf with
      | Simple _ -> <:str_item@g< >>

      | Modules (_, (_, sync_name, funcs), _) ->
          <:str_item@g<
            module Sync (A : $uid:name$.$uid:sync_name$) =
            struct
              let bind
                  ?program_number
                  ?version_number
                  srv =
                $List.fold_left
                  (fun e (_, id, _, _) ->
                    (* <:expr@g< $e$ ~ $lid:"proc" ^ id$ : A.$lid:id$ >> does not work? *)
                    Ast.ExApp(g,
                             e,
                             Ast.ExLab (g,
                                       "proc_" ^ id,
                                       <:expr@g< A.$lid:id$ >>)))
                  <:expr@g< bind ?program_number ?version_number >>
                  funcs$ srv
            end
          >> in

  match intf with
    | Simple (_, funcs)
    | Modules (_, (_, _, funcs), _) ->
        <:str_item@g<
          let bind
              ?program_number
              ?version_number =
            $List.fold_right
              (fun (_, id, _, _) e -> <:expr@g< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
              funcs
              <:expr@g<
                fun srv ->
                  Rpc_server.bind
                    ?program_number ?version_number $G.aux_val name "program"$
                    $G.conses (List.map sync_func funcs)$
                    srv
              >>$ ;;

          let bind_async
              ?program_number
              ?version_number =
            $List.fold_right
              (fun (_, id, _, _) e -> <:expr@g< fun ~ $lid:"proc_" ^ id$ -> $e$ >>)
              funcs
              <:expr@g<
                fun srv ->
                  Rpc_server.bind
                    ?program_number ?version_number $G.aux_val name "program"$
                    $G.conses (List.map async_func funcs)$
                    srv
              >>$ ;;

          $modules$
        >>
