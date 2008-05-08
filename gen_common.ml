open Camlp4.PreCast
open Ast
open S_ast
open Util

let g = Camlp4.PreCast.Loc.ghost

let arg id = id ^ "'arg"
let argi id i = id ^ "'arg" ^ string_of_int i
let res id = id ^ "'res"
let xdr id = "xdr_" ^ id
let xdr_p id = "xdr'" ^ id
let xdr_arg id = "xdr_" ^ id ^ "'arg"
let xdr_res id = "xdr_" ^ id ^ "'res"
let to_ id = "to_" ^ id
let to_p id = "to'" ^ id
let to_arg id = "to_" ^ id ^ "'arg"
let to_res id = "to_" ^ id ^ "'res"
let of_ id = "of_" ^ id
let of_p id = "of'" ^ id
let of_arg id = "of_" ^ id ^ "'arg"
let of_res id = "of_" ^ id ^ "'res"

let aux_type name id = <:ctyp@g<$uid:name ^ "_aux"$ . $lid:id$>>
let aux_val name id = <:expr@g<$uid:name ^ "_aux"$ . $lid:id$>>

let vars l =
  let ps = List.mapi (fun _ i -> <:patt@g< $lid:"x" ^ string_of_int i$ >>) l in
  let es = List.mapi (fun _ i -> <:expr@g< $lid:"x" ^ string_of_int i$ >>) l in
  (ps, es)

let arrows ts t =
  List.fold_right
    (fun t a -> <:ctyp@g< $t$ -> $a$ >>)
    ts
    t

let tapps t ts =
  List.fold_left
    (fun t t' -> <:ctyp@g< $t'$ $t$ >>)
    t
    ts

let funs ps e =
  List.fold_right
    (fun p e -> <:expr@g< fun $p$ -> $e$ >>)
    ps
    e

let funs_ids vs e =
  funs (List.map (fun v -> <:patt@g< $lid:v$ >>) vs) e

let apps e es =
  List.fold_left
    (fun e e' -> <:expr@g< $e$ $e'$ >>)
    e
    es

let conses es =
  List.fold_right
    (fun e cs -> <:expr@g< $e$ :: $cs$ >>)
    es
  <:expr@g< [] >>

let rec gen_type ?name t =
  let gen_type = gen_type ?name in
  match t with
  | Unit _ -> <:ctyp@g< unit >>
  | Int _ -> <:ctyp@g< int >>
  | Int32 _ -> <:ctyp@g< int32 >>
  | Int64 _ -> <:ctyp@g< int64 >>
  | Float _ -> <:ctyp@g< float >>
  | Bool _ -> <:ctyp@g< bool >>
  | Char _ -> <:ctyp@g< char >>
  | String _ -> <:ctyp@g< string >>

  | Var (_, v) -> <:ctyp@g< '$v$ >>

  | Tuple (_, parts) ->
      let parts = List.map gen_type parts in
      TyTup (g, tySta_of_list parts)

  | Record (_, fields) ->
      let fields =
        List.map
          (fun (id, t) -> <:ctyp@g< $lid:id$ : $gen_type t$ >>)
          fields in
      <:ctyp@g< { $tySem_of_list fields$ } >>

  | Variant (_, arms) ->
      let arms =
        List.map
          (fun (id, ts) ->
            let parts = List.map gen_type ts in
            match parts with
              | [] -> <:ctyp@g< $uid:id$ >>
              | _ -> <:ctyp@g< $uid:id$ of $tyAnd_of_list parts$ >>)
          arms in
      TySum (g, tyOr_of_list arms)

  | Array (_, t) -> <:ctyp@g< $gen_type t$ array >>
  | List (_, t) -> <:ctyp@g< $gen_type t$ list >>
  | Option (_, t) -> <:ctyp@g< $gen_type t$ option >>

  | Apply (_, id, args) ->
      List.fold_left
        (fun t a -> <:ctyp@g< $gen_type a$ $t$ >>)
        (match name with
          | None -> <:ctyp@g< $lid:id$ >>
          | Some name -> <:ctyp@g< $uid:name$.$lid:id$ >>)
        args
