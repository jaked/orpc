open Camlp4.PreCast

type ident = string

type typ =
    | Var of Loc.t * ident
    | Unit of Loc.t
    | Int of Loc.t
    | Int32 of Loc.t
    | Int64 of Loc.t
    | Float of Loc.t
    | Bool of Loc.t
    | Char of Loc.t
    | String of Loc.t
    | Tuple of Loc.t * typ list
    | Record of Loc.t * (ident * typ) list
    | Variant of Loc.t * (ident * (typ list)) list
    | Array of Loc.t * typ
    | List of Loc.t * typ
    | Option of Loc.t * typ
    | Apply of Loc.t * ident * typ list
    | Arrow of Loc.t * typ * typ

type typedef = (Loc.t * (ident list) * ident * typ) list

type func = (Loc.t * ident * (typ list) * typ)

type module_type = (Loc.t * ident * (func list))

type interface =
    | Simple of typedef list * func list
    | Modules of typedef list * module_type * module_type option

let loc_of_typ = function
  | Var (loc, _) -> loc
  | Unit loc -> loc
  | Int loc -> loc
  | Int32 loc -> loc
  | Int64 loc -> loc
  | Float loc -> loc
  | Bool loc -> loc
  | Char loc -> loc
  | String loc -> loc
  | Tuple (loc, _) -> loc
  | Record (loc, _) -> loc
  | Variant (loc, _) -> loc
  | Array (loc, _) -> loc
  | List (loc, _) -> loc
  | Option (loc, _) -> loc
  | Apply (loc, _, _) -> loc
  | Arrow (loc, _, _) -> loc

let g = Loc.ghost

(* Camlp4LocationStripper is suggestive but I can't figure out how to use it. *)
let rec strip_locs_typ = function
  | Var (_, id) -> Var (g, id)
  | Unit _ -> Unit g
  | Int _ -> Int g
  | Int32 _ -> Int32 g
  | Int64 _ -> Int64 g
  | Float _ -> Float g
  | Bool _ -> Bool g
  | Char _ -> Char g
  | String _ -> String g
  | Tuple (_, parts) -> Tuple (g, List.map strip_locs_typ parts)
  | Record (_, fields) -> Record (g, List.map (fun (id,t) -> (id, strip_locs_typ t)) fields)
  | Variant (_, arms) -> Variant (g, List.map (fun (id,ts) -> (id, List.map strip_locs_typ ts)) arms)
  | Array (_, t) -> Array (g, strip_locs_typ t)
  | List (_, t) -> List (g, strip_locs_typ t)
  | Option (_, t) -> Option (g, strip_locs_typ t)
  | Apply (_, id, args) -> Apply (g, id, List.map strip_locs_typ args)
  | Arrow (_, t1, t2) -> Arrow (g, strip_locs_typ t1, strip_locs_typ t2)
