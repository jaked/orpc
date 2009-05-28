(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
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

open Types
open Error

(* XXX check for circular type definitions. accept -rectypes? *)
(* XXX check for reserved words in type and value names and argument labels? *)
(* XXX check variant tags are not reused *)

let rec check_type ids vars btds t =
  let ct = check_type ids vars btds in
  match t with
    | Abstract _ -> ()

    | Var (loc, v) ->
        if not (List.mem v vars)
        then loc_error loc "unbound type variable"

    | Unit _ | Int _ | Int32 _ | Int64 _ | Float _ | Bool _ | Char _ | String _ -> ()
    | Tuple (_, parts) -> List.iter ct parts
    | Record (_, fields) -> List.iter (fun f -> ct f.f_typ) fields
    | Variant (_, arms) -> List.iter (fun (_, ts) -> List.iter ct ts) arms
    | Array (_, t) | List (_, t) | Option (_, t) -> ct t | Ref (_, t) -> ct t

    | Arrow (loc, _, _) -> loc_error loc "function type not supported"

    | Apply (loc, _ :: _, _, _) -> ()
        (* we don't have type info, can't check. XXX maybe should just leave checking to compiler? *)

    | Apply (loc, _, id, args) ->
        try
          let vars = List.assoc id btds in
          List.iter2
            (fun a v ->
              match a with
                | Var (loc, v') ->
                    if v <> v'
                    then loc_error loc "type constructor must be applied with same arguments as definition"
                | _ ->
                    loc_error (loc_of_typ a) "type constructor must be applied with same arguments as definition")
            args vars
        with Not_found ->
          try
            if List.assoc id ids <> List.length args
            then loc_error loc "type constructor applied with wrong arity"
            else List.iter ct args
          with Not_found -> loc_error loc "unbound type constructor"

let check_typedef ids btds { td_loc = loc; td_vars = vars; td_typ = t } =
  ignore
    (List.fold_left
        (fun vars v ->
          if List.mem v vars
          then loc_error loc "type parameter already defined"
          else v::vars)
        [] vars);
  check_type ids vars btds t

let check_typedefs ids tds =
  List.fold_left
    (fun ids ds ->
      let ids, btds =
        List.fold_left
          (fun (ids, btds) { td_loc = loc; td_vars = vars; td_id = id } ->
            if List.mem_assoc id ids
            then loc_error loc "type constructor already defined"
            else
              (id, List.length vars) :: ids, (id, vars) :: btds)
              (ids, []) ds in
      List.iter (check_typedef ids btds) ds;
      ids)
    (List.map
        (fun t -> (t, 0))
        ["unit"; "int"; "int32"; "int64"; "float"; "bool"; "char"; "string"; "array"; "list"; "option"; "exn"])
    tds

let check_excs ids excs =
  (*
    XXX is it worth checking for predefined exceptions?

    would be nice to allow equalities to existing exceptions but that
    doesn't seem to be supported in the syntax (you can have an
    equation but not a representation).

    exn is not allowed in data structures because we can't statically
    check whether an exn is something we know how to marshal.
  *)
  List.iter
    (fun (loc, _, ts) -> List.iter (check_type ids [] []) ts)
    excs

let check_function ids loc id args res =
  let args = List.map typ_of_argtyp args in
  let ct = check_type ids [] [] in
  List.iter ct args;
  ct res

let check_funcs ids fids funcs =
       ignore
         (List.fold_left
             (fun fids (loc, id, args, res) ->
               if List.mem id fids
               then loc_error loc "function already defined"
               else
                 begin
                   check_function ids loc id args res;
                   (id::fids)
                 end)
             []
             funcs)

let interface_mismatch loc msg = loc_error loc ("interface mismatch: " ^ msg)

let g = Camlp4.PreCast.Loc.ghost

let get_module_type_funcs mts =
  match mts with
    | [] -> assert false
    | (loc, kind, funcs)::_ ->
        let func =
          match kind with
            | Sync -> (fun f -> f)

            | Async ->
                (fun (loc, id, args, ret) ->
                  if not (match ret with Unit _ -> true | _ -> false)
                  then loc_error (loc_of_typ ret) "async return type must be unit";
                  let (args, reply) =
                    match List.rev args with
                      | [] -> assert false (* checked in parse *)
                      | [_] -> loc_error loc "async function must have at least two arguments"
                      | ret::args -> List.rev args, ret in
                  let ret =
                    match reply with
                      | Unlabelled (_, Arrow (_, Arrow (_, Unit _, ret), Unit _)) -> ret
                      | _ -> loc_error (loc_of_argtyp reply) "async function must have reply argument" in
                  (loc, id, args, ret))

            | Lwt ->
                (fun (loc, id, args, lwt_ret) ->
                  let ret =
                    match lwt_ret with
                      | Apply (_, ["Lwt"], "t", [ ret ]) -> ret
                      | _ -> loc_error (loc_of_typ lwt_ret) "Lwt function must return Lwt.t" in
                  (loc, id, args, ret)) in
        List.map func funcs

let check_module_type_funcs funcs (_, kind, mt_funcs) =
  let check_arg a mt_a =
    if strip_locs_argtyp a <> strip_locs_argtyp mt_a
    then interface_mismatch (loc_of_argtyp mt_a) "arg types" in
  let check_args loc args mt_args =
    try List.iter2 check_arg args mt_args
    with Invalid_argument _ -> interface_mismatch loc "arg counts" in
  let check_ret r mt_r =
    if strip_locs_typ r <> strip_locs_typ mt_r
    then interface_mismatch (loc_of_typ mt_r) "return type" in
  let check_names loc id mt_id =
    if id <> mt_id then interface_mismatch loc "names" in

  let func =
    match kind with
      | Sync ->
          (fun (_, id, args, ret) (s_loc, s_id, s_args, s_ret) ->
            check_names s_loc id s_id;
            check_args s_loc args s_args;
            check_ret ret s_ret)
      | Async ->
          (fun (_, id, args, ret) (as_loc, as_id, as_args, as_ret) ->
            check_names as_loc id as_id;
            let args = args @ [ Unlabelled (g, Arrow (g, Arrow (g, Unit g, ret), Unit g)) ] in
            check_args as_loc args as_args;
            check_ret (Unit g) as_ret)
      | Lwt ->
          (fun (_, id, args, ret) (l_loc, l_id, l_args, l_ret) ->
            check_names l_loc id l_id;
            check_args l_loc args l_args;
            check_ret (Apply (g, ["Lwt"], "t", [ ret ])) l_ret) in

  try List.iter2 func funcs mt_funcs
  with Invalid_argument _ -> interface_mismatch g "func counts"

let check_interface (typedefs, excs, funcs, mts) =
  let funcs =
    match funcs with
      | [] -> get_module_type_funcs mts
      | _ -> funcs in

  (* XXX this doesn't check that typedefs precede their uses in excs/funcs *)
  let ids = check_typedefs [] typedefs in
  check_excs ids excs;
  check_funcs ids [] funcs;

  let mode =
    match mts with
      | [] -> Simple
      | _ ->
          List.iter (check_module_type_funcs funcs) mts;
          Modules (List.map (fun (_, kind, _) -> kind) mts) in

  (typedefs, excs, funcs, mode)
