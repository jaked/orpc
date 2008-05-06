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
                  $List.foldi_right
                    (fun _ i t -> <:ctyp@g< $G.aux_type name (G.argi id i)$ -> $t$ >>)
                    args
                    (G.aux_type name (G.res id))$
                  -> $t$
                >>)
              funcs
              <:ctyp@g< Rpc_server.t -> unit >>$ ;;

          $modules$
        >>

(*
    val bind_async :
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      $List.fold_right
        (fun i t ->
          match i with
            | Function (_, id, args, _) ->
                <:ctyp@g<
                  $lid:"proc_" ^ id$ :
                  $List.foldi_right
                    (fun _ i t -> <:ctyp@g< $G.aux_type name (G.argi id i)$ -> $t$ >>)
                    args
                    <:ctyp@g< ($G.aux_type name (G.res id)$ -> unit) -> unit >>$
                  -> $t$
                >>
            | _ -> t)
        is
        <:ctyp@g< Rpc_server.t -> unit >>$ ;;
*)

let gen_srv_ml name intf =

  let sync_func (_, id, args, _) e =
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
                      $List.fold_left
                      (fun e a -> <:expr@g< $e$ $a$ >>)
                      <:expr@g< $lid:"proc_" ^ id$ >>
                      es$
                >>$
      } ::$e$
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
                    $List.fold_right sync_func funcs <:expr@g< [] >>$
                    srv
              >>$ ;;

          $modules$
        >>
