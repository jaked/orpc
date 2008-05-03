open Camlp4.PreCast
open Ast
open S_ast
open Error

let rec parse_type t =
  match t with
    | <:ctyp@loc< unit >> -> Unit loc
    | <:ctyp@loc< int >> -> Int loc
    | <:ctyp@loc< int32 >> -> Int32 loc
    | <:ctyp@loc< int64 >> -> Int64 loc
    | <:ctyp@loc< float >> -> Float loc
    | <:ctyp@loc< bool >> -> Bool loc
    | <:ctyp@loc< char >> -> Char loc
    | <:ctyp@loc< string >> -> String loc

    | <:ctyp@loc< '$v$ >> -> Var (loc, v)
    | <:ctyp@loc< $lid:id$ >> -> Apply (loc, id, [])

    (* I don't see how to do this one with quotations; $t1$ * $t2$
       gives both the TyTup and the TySta *)
    | TyTup (loc, ts) ->
        let rec parts = function
          | TySta (_, t1, t2) -> parts t1 @ parts t2
          | TyTup (_, t) -> parts t
          | t -> [ parse_type t ] in
        Tuple (loc, parts ts)

    | <:ctyp@loc< { $fs$ } >> ->
      let rec fields = function
        | <:ctyp< $t1$; $t2$ >> -> fields t1 @ fields t2
        | <:ctyp< $lid:id$ : $t$ >> -> [ id, parse_type t ]
        | t -> ctyp_error t "expected TySem or TyCol" in
      Record (loc, fields fs)

    (* syntax for TySum? *)
    | TySum (loc, ams) ->
        let rec arms = function
          | <:ctyp< $t1$ | $t2$ >> -> arms t1 @ arms t2
          | <:ctyp< $uid:id$ of $t$ >> ->
              let rec parts = function
                | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
                | t -> [ parse_type t ] in
              [ id, parts t ]
          | <:ctyp< $uid:id$ >> -> [ id, [] ]
          | t -> ctyp_error t "expected TyOr, TyOf, or TyId" in
        Variant (loc, arms ams)

    | <:ctyp@loc< $t$ array >> -> Array (loc, parse_type t)
    | <:ctyp@loc< $t$ list >> -> List (loc, parse_type t)
    | <:ctyp@loc< $t$ option >> -> Option (loc, parse_type t)

    | <:ctyp@loc< $_$ $_$ >> ->
        let rec apps args = function
            (* TyApp is used for both tupled and nested type application *)
          | <:ctyp< $t2$ $t1$ >> -> apps (parse_type t2 :: args) t1
          | <:ctyp< $lid:id$ >> -> Apply (loc, id, args)
          | t -> ctyp_error t "expected TyApp or TyId" in
        apps [] t

    | TyMan (_, _, t) -> parse_type t

    | t -> ctyp_error t "unsupported type"

let parse_typedef loc t =
  let rec types t a =
    match t with
      | TyAnd (_, t1, t2) -> types t1 (types t2 a)
      | TyDcl (loc, id, tvars, t, []) ->
          let tvars =
            List.map
              (function
                | TyQuo (_, v) -> v
                | t -> ctyp_error t "expected type variable")
              tvars in
          let t = parse_type t in
          (loc, tvars, id, t) ::a
    | t -> ctyp_error t "expected type declaration" in
  types t []

let parse_val loc id t =
  let rec args t a =
    match t with
      | TyArr (_, t1, t2) -> parse_type t1 :: args t2 a
      | t -> parse_type t ::a in
  begin
    match args t [] with
      | []
      | [_] -> loc_error loc "function must have at least one argument"
      | args ->
          let rargs = List.rev args in
          (loc, id, List.rev (List.tl rargs), List.hd rargs)
  end

type s = {
  typedefs : typedef list;
  funcs : func list;
  module_types : module_type list;
}

let rec parse_sig_items i a =
  match i with
    | SgNil _ -> a
    | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
    | SgTyp (loc, t) -> { a with typedefs = parse_typedef loc t ::a.typedefs }
    | SgVal (loc, id, t) -> { a with funcs = parse_val loc id t ::a.funcs }
    | SgMty (loc, id, MtSig (_, i)) -> { a with module_types = parse_module_type loc id i ::a.module_types }
    | i -> sig_item_error i "expected type, function declaration, or module type"

and parse_module_type loc id i =
  let rec parse_sig_items i a =
    match i with
      | SgNil _ -> a
      | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
      | SgVal (loc, id, t) -> parse_val loc id t ::a
      | i -> sig_item_error i "expected function declaration" in
  (loc, id, parse_sig_items i [])

let parse_interface i =
  let a = { typedefs = []; funcs = []; module_types = [] } in
  match parse_sig_items i a with
    | { typedefs = tds; funcs = []; module_types = [ sync ] } -> Modules (tds, sync, None)
    | { typedefs = tds; funcs = []; module_types = [ sync; async ] } -> Modules (tds, sync, Some async)
    | { typedefs = tds; funcs = funcs; module_types = [] } -> Simple (tds, funcs)
    | _ -> loc_error Camlp4.PreCast.Loc.ghost "expected simple interface or modules interface"
