open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let gen_clnt_mli name (typedefs, excs, funcs, kinds) =

  let modules =
    List.map
      (function
        | Sync ->
            <:sig_item<
              module Sync(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) : $uid:name$.Sync
            >>
        | Async ->
            <:sig_item<
              module Async(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) : $uid:name$.Async
            >>
        | Lwt ->
            <:sig_item<
              module Lwt(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) : $uid:name$.Lwt
            >>)
      kinds in

  <:sig_item<
    val create_client :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      Rpc_client.connector ->
      Rpc.protocol ->
      Rpc_client.t

    val create_portmapped_client :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      string ->
      Rpc.protocol ->
      Rpc_client.t

    val create_client2 :
      ?esys:Unixqueue.event_system ->
      ?program_number:Rtypes.uint4 ->
      ?version_number:Rtypes.uint4 ->
      Rpc_client.mode2 ->
      Rpc_client.t ;;

    $sgSem_of_list
      (List.map
          (fun (_, id, args, res) ->
            <:sig_item<
              val $lid:id$ : Rpc_client.t ->
                $G.arrows
                  (List.mapi
                      (fun a i -> G.labelled_ctyp a (G.aux_type name (G.argi id i)))
                      args)
                  (G.aux_type name (G.res0 id))$
            >>)
          funcs)$ ;;

    $sgSem_of_list
      (List.map
          (fun (_, id, args, res) ->
            <:sig_item<
              val $lid:id ^ "'async"$ : Rpc_client.t ->
                $G.arrows
                  (List.mapi
                      (fun a i -> G.labelled_ctyp a (G.aux_type name (G.argi id i)))
                      args)
                  <:ctyp< ((unit -> $G.aux_type name (G.res0 id)$) -> unit) -> unit >>$
            >>)
          funcs)$ ;;

    $sgSem_of_list modules$
  >>

let gen_clnt_ml name (typedefs, excs, funcs, kinds) =

  let sync_func ~has_excs (_, id, args, res) =
    (fun body ->
      <:str_item<
        let $lid:id$ = fun client ->
          $match args with
            | [] -> assert false
            | [a] -> <:expr< fun $G.labelled_patt a <:patt< arg >>$ -> $body$ >>
            | _ ->
                let (ps, es) = G.vars args in
                G.funs
                  (List.map2 G.labelled_patt args ps)
                  <:expr< let arg = ($exCom_of_list es$) in $body$ >>$
      >>)
      ((fun body2 ->
          if has_excs
          then <:expr< Orpc.unpack_orpc_result $body2$ >>
          else body2)
        <:expr<
          $G.aux_val name (G.to_res id)$
          (Rpc_client.sync_call client $`str:id$ ($G.aux_val name (G.of_arg id)$ arg))
        >>) in

  let async_func ~has_excs (_, id, args, res) =
    (fun body ->
      <:str_item<
        let $lid:id ^ "'async"$ = fun client ->
          $match args with
            | [] -> assert false
            | [a] -> <:expr< fun $G.labelled_patt a <:patt< arg >>$ pass_reply -> $body$ >>
            | _ ->
                let (ps, es) = G.vars args in
                G.funs
                  (List.map2 G.labelled_patt args ps)
                  <:expr< fun pass_reply ->
                    let arg = ($exCom_of_list es$) in $body$
                  >>$
      >>)
      (if has_excs
      then
        <:expr<
          Rpc_client.add_call client $`str:id$ ($G.aux_val name (G.of_arg id)$ arg)
          (fun g -> pass_reply (fun () -> Orpc.unpack_orpc_result ($G.aux_val name (G.to_res id)$ (g ()))))
        >>
      else
        <:expr<
          Rpc_client.add_call client $`str:id$ ($G.aux_val name (G.of_arg id)$ arg)
          (fun g -> pass_reply (fun () -> $G.aux_val name (G.to_res id)$ (g ())))
        >>) in

  let modules =
    List.map
      (function
        | Sync ->
            let func (_, id, args, res) =
              let (ps, es) = G.vars args in
              <:str_item<
                let $lid:id$ =
                  $G.funs
                    (List.map2 G.labelled_patt args ps)
                    <:expr<
                      C.with_client
                        (fun c ->
                          $G.apps
                            <:expr< $lid:id$ c >>
                            (List.map2 G.labelled_expr args es)$)
                    >>$
              >> in

            <:str_item<
              module Sync(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) =
              struct
                $stSem_of_list (List.map func funcs)$
              end
            >>

        | Async ->
            let func (_, id, args, res) =
              let (ps, es) = G.vars args in
              <:str_item<
                let $lid:id$ =
                  $G.funs
                    (List.map2 G.labelled_patt args ps)
                    <:expr<
                      fun pass_reply ->
                        C.with_client
                          (fun c ->
                            $G.apps
                              <:expr< $lid:id ^ "'async"$ c >>
                              (List.map2 G.labelled_expr args es)$
                            pass_reply)
                    >>$
              >> in

            <:str_item<
              module Async(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) =
              struct
                $stSem_of_list (List.map func funcs)$
              end
            >>

        | Lwt ->
            let func (_, id, args, res) =
              let (ps, es) = G.vars args in
              <:str_item<
                let $lid:id$ =
                  $G.funs
                    (List.map2 G.labelled_patt args ps)
                    <:expr<
                        C.with_client
                          (fun c ->
                            let res = Lwt.wait () in
                            $G.apps
                              <:expr< $lid:id ^ "'async"$ c >>
                              (List.map2 G.labelled_expr args es)$
                            (fun r ->
                              try Lwt.wakeup res (r ())
                              with exn -> Lwt.wakeup_exn res exn);
                            res)
                    >>$
              >> in

            <:str_item<
              module Lwt(C : sig val with_client : (Rpc_client.t -> 'a) -> 'a end) =
              struct
                $stSem_of_list (List.map func funcs)$
              end
            >>)

      kinds in

  let has_excs = excs <> [] in

  <:str_item<
    let create_client
        ?(esys = Unixqueue.create_unix_event_system())
        ?program_number
        ?version_number
        connector
        protocol =
      Rpc_client.create ?program_number ?version_number esys connector protocol $G.aux_val name "program"$

    let create_portmapped_client ?esys ?program_number ?version_number host protocol =
      create_client ?esys ?program_number ?version_number (Rpc_client.Portmapped host) protocol

    let create_client2
        ?(esys = Unixqueue.create_unix_event_system())
        ?program_number
        ?version_number
        mode2 =
      Rpc_client.create2 ?program_number ?version_number mode2 $G.aux_val name "program"$ esys ;;

    $stSem_of_list (List.map (sync_func ~has_excs) funcs)$ ;;

    $stSem_of_list (List.map (async_func ~has_excs) funcs)$ ;;

    $stSem_of_list modules$
  >>
