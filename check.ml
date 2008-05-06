open S_ast
open Error

(* XXX check for circular type definitions. accept -rectypes? *)
let rec check_type ids vars t =
  let ct = check_type ids vars in
  match t with
    | Var (loc, v) ->
        begin
          match vars with
            | Some vars ->
                if not (List.mem v vars)
                then loc_error loc "unbound type variable"
            | None -> loc_error loc "type variables not allowed in functions"
        end

    | Unit _ | Int _ | Int32 _ | Int64 _ | Float _ | Bool _ | Char _ | String _ -> ()
    | Tuple (_, parts) -> List.iter ct parts
    | Record (_, fields) -> List.iter (fun (_, t) -> ct t) fields
    | Variant (_, arms) -> List.iter (fun (_, ts) -> List.iter ct ts) arms
    | Array (_, t) | List (_, t) | Option (_, t) -> ct t

    | Apply (loc, id, args) ->
        begin
          try
            if List.assoc id ids <> List.length args
            then loc_error loc "type constructor applied with wrong arity"
            else List.iter ct args
          with Not_found -> loc_error loc "unbound type constructor"
        end

let check_typedef ids (loc, vars, id, t) =
  ignore
    (List.fold_left
        (fun vars v ->
          if List.mem v vars
          then loc_error loc "type parameter already defined"
          else v::vars)
        [] vars);
  check_type ids (Some vars) t

let check_typedefs ids tds =
  List.fold_left
    (fun ids ds ->
      let ids =
        List.fold_left
          (fun ids (loc, vars, id, _) ->
            if List.mem_assoc id ids
            then loc_error loc "type constructor already defined"
            else
              (id, List.length vars) :: ids)
              ids ds in
      List.iter (check_typedef ids) ds;
      ids)
    (List.map
        (fun t -> (t, 0))
        ["unit"; "int"; "int32"; "int64"; "float"; "bool"; "char"; "string"; "array"; "list"; "option"])
    tds

let check_function ids loc id args res =
  let ct = check_type ids None in
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

let check_interface = function
  | Simple (typedefs, funcs)
  | Modules (typedefs, (_, _, funcs), _) ->
      let ids = check_typedefs [] typedefs in
      check_funcs ids [] funcs
      (* XXX check async against sync *)
