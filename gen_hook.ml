open Camlp4.PreCast
open Ast
open S_ast
open Util

module G = Gen_common

let _loc = Camlp4.PreCast.Loc.ghost

let gen_hook_mli name (typedefs, excs, funcs, kinds) =

  let modules =
    List.map
      (function
        | Sync ->
            <:sig_item@g<
              module Sync
                (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
                (A : $uid:name$.Sync) : $uid:name$.Sync
            >>
        | Async ->
            <:sig_item@g<
              module Async
                (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
                (A : $uid:name$.Async) : $uid:name$.Async
            >>
        | Lwt ->
            <:sig_item@g<
              module Lwt
                (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
                (A : $uid:name$.Lwt) : $uid:name$.Lwt
            >>)
      kinds in

  <:sig_item<
    $sgSem_of_list modules$
  >>

let gen_hook_ml name (typedefs, excs, funcs, kinds) =

  let modules =
    ListLabels.map kinds ~f:(function

      | Sync ->
          let func (_, id, args, res) =
            <:str_item<
              let $lid:id$ =
                $G.args_funs args
                  <:expr<
                    let ok, exn = H.hook $`str:id$ in
                    try
                      let r = $G.args_apps <:expr< A.$lid:id$ >> args$ in
                      (try ok () with _ -> ());
                      r
                    with e ->
                      (try exn e with _ -> ());
                      raise e
                  >>$
            >> in
          <:str_item<
            module Sync
              (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
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
                      let ok, exn = H.hook $`str:id$ in
                      let pass_reply rf =
                        (try let r = rf () in (try ok () with _ -> ())
                          with e -> (try exn e with _ -> ()));
                        pass_reply rf in
                      $G.args_apps <:expr< A.$lid:id$ >> args$ pass_reply
                  >>$
            >> in
          <:str_item<
            module Async
              (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
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
                    let ok, exn = H.hook $`str:id$ in
                    Lwt.try_bind
                      (fun () -> $G.args_apps <:expr< A.$lid:id$ >> args$)
                      (fun r -> (try ok () with _ -> ()); Lwt.return r)
                      (fun e -> (try exn e with _ -> ()); Lwt.fail e)
                  >>$
            >> in
          <:str_item<
            module Lwt
              (H : sig val hook : string -> (unit -> unit) * (exn -> unit) end)
              (A : $uid:name$.Lwt) =
            struct
              $stSem_of_list (List.map func funcs)$
            end
          >>) in

  <:str_item<
    $stSem_of_list modules$
  >>
