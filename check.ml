open S_ast
open Error

(* XXX check for circular type definitions. accept -rectypes? *)
let rec check_type ids vars btds t =
  let ct = check_type ids vars btds in
  match t with
    | Var (loc, v) ->
        if not (List.mem v vars)
        then loc_error loc "unbound type variable"

    | Unit _ | Int _ | Int32 _ | Int64 _ | Float _ | Bool _ | Char _ | String _ -> ()
    | Tuple (_, parts) -> List.iter ct parts
    | Record (_, fields) -> List.iter (fun (_, t) -> ct t) fields
    | Variant (_, arms) -> List.iter (fun (_, ts) -> List.iter ct ts) arms
    | Array (_, t) | List (_, t) | Option (_, t) -> ct t

    | Arrow (loc, _, _) -> loc_error loc "function type not supported"

    | Apply (loc, id, args) ->
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

let check_typedef ids btds (loc, vars, id, t) =
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
          (fun (ids, btds) (loc, vars, id, _) ->
            if List.mem_assoc id ids
            then loc_error loc "type constructor already defined"
            else
              (id, List.length vars) :: ids, (id, vars) :: btds)
              (ids, []) ds in
      List.iter (check_typedef ids btds) ds;
      ids)
    (List.map
        (fun t -> (t, 0))
        ["unit"; "int"; "int32"; "int64"; "float"; "bool"; "char"; "string"; "array"; "list"; "option"])
    tds

let check_function ids loc id args res =
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

let async_mismatch loc msg = loc_error loc ("sync/async interface mismatch: " ^ msg)

let g = Camlp4.PreCast.Loc.ghost

let check_async (s_loc, s_id, s_args, s_ret) (as_loc, as_id, as_args, as_ret) =
  if s_id <> as_id then async_mismatch as_loc "names";
  if not (match as_ret with Unit _ -> true | _ -> false)
  then async_mismatch (loc_of_typ as_ret) "return type";
  let check_arg sync async =
    if strip_locs_typ sync <> strip_locs_typ async
    then async_mismatch (loc_of_typ async) "arg types" in
  try List.iter2 check_arg (s_args @ [ Arrow (g, s_ret, Unit g) ]) as_args
  with Invalid_argument _ -> async_mismatch as_loc "arg counts"

let check_interface i =
  begin
    match i with
      | Simple (typedefs, funcs)
      | Modules (typedefs, (_, _, funcs), _) ->
          let ids = check_typedefs [] typedefs in
          check_funcs ids [] funcs
  end;
  begin
    match i with
      | Modules (_, (_, _, funcs), Some (_, _, async_funcs)) ->
          begin
            try List.iter2 check_async funcs async_funcs
            with Invalid_argument _ -> async_mismatch g "func counts"
          end
      | _ -> ()
  end
