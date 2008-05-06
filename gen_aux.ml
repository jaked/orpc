open Camlp4.PreCast
open Ast
open S_ast
open Util

let g = Camlp4.PreCast.Loc.ghost

module G = Gen_common

let rec strip_locs = function
  | Var (_, id) -> Var (g, id)
  | Unit _ -> Unit g
  | Int _ -> Int g
  | Int32 _ -> Int32 g
  | Int64 _ -> Int64 g
  | Float _ -> Float g
  | Bool _ -> Bool g
  | Char _ -> Char g
  | String _ -> String g
  | Tuple (_, parts) -> Tuple (g, List.map strip_locs parts)
  | Record (_, fields) -> Record (g, List.map (fun (id,t) -> (id, strip_locs t)) fields)
  | Variant (_, arms) -> Variant (g, List.map (fun (id,ts) -> (id, List.map strip_locs ts)) arms)
  | Array (_, t) -> Array (g, strip_locs t)
  | List (_, t) -> List (g, strip_locs t)
  | Option (_, t) -> Option (g, strip_locs t)
  | Apply (_, id, args) -> Apply (g, id, List.map strip_locs args)

let id_digest id args =
  match args with
    | [] -> id
    | _ ->
        (* must strip locs since they affect digest *)
        let args = List.map strip_locs args in
        id ^ "_" ^ Digest.to_hex (Digest.string (S_ast.Show_typ.show_list args))

let gen_aux_mli name intf =
  let gen_typedef ?name ds is =
    let is =
      List.fold_right
        (fun (_, vars, id, _) is ->
          let appd =
            List.fold_left
              (fun t v -> <:ctyp@g< '$lid:v$ $t$ >>)
              (match name with
                | None -> <:ctyp@g< $lid:id$ >>
                | Some name -> <:ctyp@g< $uid:name$.$lid:id$ >>)
              vars in

          let t =
            List.fold_right
              (fun v t -> <:ctyp@g< (Xdr.xdr_value -> '$lid:v$) -> $t$ >>)
              vars
              <:ctyp@g< Xdr.xdr_value -> $appd$ >> in
          let is = <:sig_item@g< val $lid:G.to_ id$ : $t$ >> ::is in

          let t =
            List.fold_right
              (fun v t -> <:ctyp@g< ('$lid:v$ -> Xdr.xdr_value) -> $t$ >>)
              vars
              <:ctyp@g< $appd$ -> Xdr.xdr_value >> in
          let is = <:sig_item@g< val $lid:G.of_ id$ : $t$ >> ::is in

          let t =
            List.fold_right
              (fun v t -> <:ctyp@g< Xdr.xdr_type_term -> $t$ >>)
              vars
              <:ctyp@g< Xdr.xdr_type_term >> in
          let is = <:sig_item@g< val $lid:G.xdr id$ : $t$ >> ::is in
          is)
        ds is in

    match name with
      | None ->
          let ts =
            List.map
              (fun (_, vars, id, t) ->
                let vars = List.map (fun v -> TyQuo (g, v)) vars in
                TyDcl (g, id, vars, G.gen_type t, []))
              ds in
          SgTyp (g, tyAnd_of_list ts) :: is

      | Some _ -> is in

  let gen_func ?name (_, id, args, res) is =
    let arg =
      match args with
        | [] -> assert false
        | [a] -> a
        | _ -> Tuple (g, args) in
    let items aid arg is =
      <:sig_item@g<
        type $lid:aid$ = $G.gen_type ?name arg$
        val $lid:G.to_ aid$ : Xdr.xdr_value -> $lid:aid$
        val $lid:G.of_ aid$ : $lid:aid$ -> Xdr.xdr_value
        val $lid:G.xdr aid$ : Xdr.xdr_type_term
      >> :: is in
    let is = items (G.res id) res is in
    let is = items (G.arg id) arg is in
    let is =
      List.foldi_right
        (fun arg i is -> <:sig_item@g< type $lid:G.argi id i$ = $G.gen_type ?name arg$ >> :: is)
        args
        is in
    is in

  match intf with
    | Simple (typedefs, funcs) ->
        sgSem_of_list
          (List.fold_right gen_typedef typedefs
              (List.fold_right gen_func funcs
                  [ <:sig_item@g< val program : Rpc_program.t >> ]))

    | Modules (typedefs, (_, _, funcs), _) ->
        sgSem_of_list
          (List.fold_right (gen_typedef ~name) typedefs
              (List.fold_right (gen_func ~name) funcs
                  [ <:sig_item@g< val program : Rpc_program.t >> ]))

let rec gen_to ?name t x =
  let gen_to = gen_to ?name in
  match t with
    | Var (_, id) -> <:expr@g< $lid:G.to_p id$ $x$ >>
    | Unit _ -> <:expr@g< () >>
    | Int _ -> <:expr@g< Rtypes.int_of_int4 (Xdr.dest_xv_int $x$) >>
    | Int32 _ -> <:expr@g< Rtypes.int32_of_int4 (Xdr.dest_xv_int $x$) >>
    | Int64 _ -> <:expr@g< Rtypes.int64_of_int8 (Xdr.dest_xv_hyper $x$) >>
    | Float _ -> <:expr@g< Rtypes.float_of_fp8 (Xdr.dest_xv_double $x$) >>
    | Bool _ -> <:expr@g< Xdr.dest_xv_enum_fast $x$ = 1 >>
    | Char _ -> <:expr@g< char_of_int (Xdr.dest_xv_enum_fast $x$) >>
    | String _ -> <:expr@g< Xdr.dest_xv_string >>

    | Tuple (_, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr@g<
          match Xdr.dest_xv_struct_fast $x$ with
            | [| $paSem_of_list pps$ |] -> ( $exCom_of_list (List.map2 gen_to parts pes)$ )
            | _ -> assert false
        >>

    | Record (_, fields) ->
        let (fps, fes) = G.vars fields in
        let rb (id, t) e =
          match name with
            | None -> <:rec_binding@g< $lid:id$ = $gen_to t e$ >>
            | Some name -> <:rec_binding@g< $uid:name$.$lid:id$ = $gen_to t e$ >> in
        <:expr@g<
          match Xdr.dest_xv_struct_fast $x$ with
            | [| $paSem_of_list fps$ |] -> $ExRec(g, rbSem_of_list (List.map2 rb fields fes), ExNil g)$
            | _ -> assert false
        >>

     | Variant (_, arms) ->
         let mc (id, ts) i =
           match ts with
             | [] ->
                 begin
                   match name with
                     | None -> <:match_case@g< ($`int:i$, _) -> $uid:id$ >>
                     | Some name -> <:match_case@g< ($`int:i$, _) -> $uid:name$.$uid:id$ >>
                 end
             | [t] ->
                 begin
                   match name with
                     | None -> <:match_case@g< ($`int:i$, x) -> $uid:id$ $gen_to t <:expr@g< x >>$ >>
                     | Some name -> <:match_case@g< ($`int:i$, x) -> $uid:name$.$uid:id$ $gen_to t <:expr@g< x >>$ >>
                 end
             | _ ->
                 let (pps, pes) = G.vars ts in
                 <:match_case@g<
                   ($`int:i$, x) ->
                     match Xdr.dest_xv_struct_fast x with
                       | [| $paSem_of_list pps$ |] ->
                           $List.fold_left
                             (fun ps p -> <:expr@g< $ps$ $p$ >>)
                             (match name with
                               | None -> <:expr@g< $uid:id$ >>
                               | Some name -> <:expr@g< $uid:name$.$uid:id$ >>)
                             (List.map2 gen_to ts pes)$
                       | _ -> assert false
                 >> in
         ExMat (g, <:expr@g< Xdr.dest_xv_union_over_enum_fast $x$ >>,
               mcOr_of_list (List.mapi mc arms @ [ <:match_case@g< _ -> assert false >> ]))

     | Array (_, t) ->
         <:expr@g< Array.map (fun x -> $gen_to t <:expr@g< x >>$) (Xdr.dest_xv_array $x$) >>

     | List (_, t) ->
         <:expr@g<
           let rec loop x =
             match Xdr.dest_xv_union_over_enum_fast x with
               | (0, _) -> []
               | (1, x) ->
                   (match Xdr.dest_xv_struct_fast x with
                     | [| x0; x1 |] -> $gen_to t <:expr@g< x0 >>$ :: loop x1
                     | _ -> assert false)
               | _ -> assert false in
           loop $x$
         >>

     | Option (_, t) ->
         <:expr@g<
           match Xdr.dest_xv_union_over_enum_fast $x$ with
             | (0, _) -> None
             | (1, x) -> Some $gen_to t <:expr@g< x >>$
             | _ -> assert false
         >>

     | Apply (_, id, args) ->
         <:expr@g<
           $List.fold_left
             (fun e a -> <:expr@g< $e$ (fun x -> $gen_to a <:expr@g< x >>$) >>)
             <:expr@g< $lid:G.to_ id$ >>
             args$
           $x$
         >>

let rec gen_of ?name t v =
  let gen_of = gen_of ?name in
  match t with
    | Var (_, id) -> <:expr@g< $lid:G.of_p id$ $v$ >>
    | Unit _ -> <:expr@g< Xdr.XV_void >>
    | Int _ -> <:expr@g< Xdr.XV_int (Rtypes.int4_of_int $v$) >>
    | Int32 _ -> <:expr@g< Xdr.XV_int (Rtypes.int4_of_int32 $v$) >>
    | Int64 _ -> <:expr@g< Xdr.XV_hyper (Rtypes.int8_of_int64 $v$) >>
    | Float _ -> <:expr@g< Xdr.XV_double (Rtypes.fp8_of_float $v$) >>
    | Bool _ -> <:expr@g< Xdr.XV_enum_fast (if $v$ then 1 else 0) >>
    | Char _ -> <:expr@g< Xdr.XV_enum_fast (int_of_char $v$) >>
    | String _ -> <:expr@g< Xdr.XV_string $v$ >>

    | Tuple (_, parts) ->
        let (pps, pes) = G.vars parts in
        <:expr@g<
          let ( $paCom_of_list pps$ ) = $v$ in
          Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of parts pes)$ |]
        >>

    | Record (_, fields) ->
        let (fps, fes) = G.vars fields in
        let rb (id, _) p =
          match name with
            | None -> <:patt@g< $lid:id$ = $p$ >>
            (* | Some name -> <:patt@g< $uid:name$.$lid:id$ = $p$ >> *)(* doesn't work *)
            | Some name -> <:patt@g< $Ast.IdAcc(g, Ast.IdUid (g, name), Ast.IdLid (g, id))$ = $p$ >> in
        <:expr@g<
          let { $paSem_of_list (List.map2 rb fields fps)$ } = $v$ in
          Xdr.XV_struct_fast [| $exSem_of_list (List.map2 (fun (_, t) v -> gen_of t v) fields fes)$ |]
        >>

     | Variant (_, arms) ->
         let mc (id, ts) i =
           match ts with
             | [] ->
                 begin
                   match name with
                     | None -> <:match_case@g< $uid:id$ -> Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void) >>
                     | Some name -> <:match_case@g< $uid:name$.$uid:id$ -> Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void) >>
                 end
             | [t] ->
                 begin
                   match name with
                     | None -> <:match_case@g< $uid:id$ x -> Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr@g< x >>$) >>
                     | Some name -> <:match_case@g< $uid:name$.$uid:id$ x -> Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr@g< x >>$) >>
                 end
             | _ ->
                 let (pps, pes) = G.vars ts in
                 match name with
                   | None ->
                       <:match_case@g<
                         $uid:id$ ( $paCom_of_list pps$ ) ->
                           Xdr.XV_union_over_enum_fast
                             ($`int:i$,
                             Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                       >>
                   | Some name ->
                       <:match_case@g<
                         $uid:name$.$uid:id$ ( $paCom_of_list pps$ ) ->
                           Xdr.XV_union_over_enum_fast
                             ($`int:i$,
                             Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                       >> in
         ExMat (g, <:expr@g< $v$ >>,
               mcOr_of_list (List.mapi mc arms))

     | Array (_, t) ->
         <:expr@g< Xdr.XV_array (Array.map (fun v -> $gen_of t <:expr@g< v >>$) $v$) >>

     | List (_, t) ->
         <:expr@g<
           let rec loop v =
             match v with
               | [] -> Xdr.XV_union_over_enum_fast (0, Xdr.XV_void)
               | v0::v1 -> Xdr.XV_union_over_enum_fast (1, Xdr.XV_struct_fast [| $gen_of t <:expr@g< v0 >>$; loop v1 |]) in
           loop $v$
         >>

     | Option (_, t) ->
         <:expr@g<
           match $v$ with
             | None -> (0, Xdr.XV_void)
             | Some v -> (1, $gen_of t <:expr@g< v >>$)
         >>

     | Apply (_, id, args) ->
         <:expr@g<
           $List.fold_left
             (fun e a -> <:expr@g< $e$ (fun v -> $gen_of a <:expr@g< v >>$) >>)
             <:expr@g< $lid:G.of_ id$ >>
             args$
           $v$
         >>

(*
  this is kind of hairy because Xdr.xdr_type_term doesn't have a nice
  way to express mutual recursion and polymorphism. the basic problem
  is that while X_rec / X_refer give you a way to tie a single
  recursive loop, for forward references in mutual recursion you have
  to inline a copy of the term you're referring to. polymorphism
  complicates things: ordinarily we express it at the OCaml level, but
  when we inline a term we have to instantiate it explicitly, and we must
  distinguish between different instantiations.

  some ways out of this (requiring Xdr modifications):

    1. provide explicit xdr_type_terms for mutually-recursive bundles
       and projection out of a bundle, and for type abstraction and
       instantiation.

    2. do everything at the OCaml level, no X_rec / X_refer. for this we
       need a way to make circular data structures but be able to recurse
       over them (see how deriving does it in Typeable).

  vs is an environment giving instantiations for type variables.
  bs is an environment listing the bound constructors (i.e. we're in the scope of an X_rec)
  ds is an environment defining constructors for forward inlining
*)

let rec gen_xdr vs bs ds t =
  let gx = gen_xdr vs bs ds in

  match t with
    | Var (_, id) ->
        begin
          try List.assoc id vs
          with Not_found -> <:expr@g< $lid:G.xdr_p id$ >>
        end

    | Unit _ -> <:expr@g< Xdr.X_void >>
    | Int _ -> <:expr@g< Xdr.X_int >>
    | Int32 _ -> <:expr@g< Xdr.X_int >>
    | Int64 _ -> <:expr@g< Xdr.X_hyper >>
    | Float _ -> <:expr@g< Xdr.X_double >>
    | Bool _ -> <:expr@g< Xdr.x_bool >>
    | Char _ -> <:expr@g<
        Xdr.X_enum (let rec loop n =
                      if n = 256 then []
                      else (String.make 1 (char_of_int n), Rtypes.int4_of_int n)::loop (n + 1) in
                    loop 0)
        >>

    | String _ -> <:expr@g< Xdr.X_string_max >>

    | Tuple (_, parts) ->
        let px t i = <:expr@g< ( $`str:string_of_int i$, $gx t$ ) >> in
        <:expr@g< Xdr.X_struct [ $exSem_of_list (List.mapi px parts)$ ] >>

    | Record (_, fields) ->
        let fx (id, t) = <:expr@g< ( $lid:id$, $gx t$ ) >> in
        <:expr@g< Xdr.X_struct [ $exSem_of_list (List.map fx fields)$ ] >>

    | Variant (_, arms) ->
        let tag (id, _) i = <:expr@g< ( $`str:id$, Rtypes.int4_of_int $`int:i$ ) >> in
        let ax (id, ts) =
          match ts with
            | [] -> <:expr@g< ( $`str:id$, Xdr.X_void ) >>
            | [t] -> <:expr@g< ( $`str:id$,  $gx t$ ) >>
            | _ ->
                let px t i = <:expr@g< ( $`str:string_of_int i$, $gx t$ ) >> in
                <:expr@g< ( $`str:id$, Xdr.X_struct [ $exSem_of_list (List.mapi px ts)$ ]) >> in
        <:expr@g<
          Xdr.X_union_over_enum
            (Xdr.X_enum [ $exSem_of_list (List.mapi tag arms)$ ],
            [ $exSem_of_list (List.map ax arms)$ ],
            None)
        >>

    | Array (_, t) ->
        <:expr@g< Xdr.x_array_max $gx t$ >>

    | List (_, t) ->
        <:expr@g<
          Xdr.X_rec ("list",
                    Xdr.X_union_over_enum
                      (Xdr.x_bool,
                      ["FALSE", Xdr.X_void;
                       "TRUE", Xdr.X_struct ["0", $gx t$; "1", Xdr.X_refer "list"]],
                      None))
        >>

    | Option (_, t) ->
        <:expr@g< Xdr.x_optional $gx t$ >>

    | Apply (_, id, args) ->
        let bid = id_digest id args in

        if List.mem bid bs
        (* refer to a def in scope with same args. *)
        then <:expr@g< Xdr.X_refer $`str:bid$ >>

        else begin
          try
            let (_, vars, _, t) = List.find (fun (_,_,id',_) -> id' = id) ds in
            (* inline / instantiate a forward def. can just replace vs because defs have no free variables. *)
            gen_xdr_def (List.combine vars (List.map gx args)) bs ds bid t

          with Not_found ->
            (* refer to a previous def at the OCaml level *)
            List.fold_left
              (fun e a -> <:expr@g< $e$ $gx a$ >>)
              <:expr@g< $lid:G.xdr id$ >>
              args
        end

and gen_xdr_def vs bs ds id t =
  <:expr@g< Xdr.X_rec ($`str:id$, $gen_xdr vs (id::bs) ds t$) >>

let gen_aux_ml name intf =
  let gen_typedef ?name ds is =
    let es =
      List.map
        (fun (_, vars, id, t) ->
          <:binding@g<
            $lid:G.to_ id$ =
            $List.fold_right
              (fun v e -> <:expr@g< fun $lid:G.to_p v$ -> $e$ >>)
              vars
              <:expr@g< fun x -> $gen_to ?name t <:expr@g< x >>$ >>$
          >>)
        ds in
    let is = StVal (g, BTrue, biAnd_of_list es) :: is in

    let es =
      List.map
        (fun (_, vars, id, t) ->
          <:binding@g<
            $lid:G.of_ id$ =
            $List.fold_right
              (fun v e -> <:expr@g< fun $lid:G.of_p v$ -> $e$ >>)
              vars
              <:expr@g< fun x -> $gen_of ?name t <:expr@g< x >>$ >>$
          >>)
        ds in
    let is = StVal (g, BTrue, biAnd_of_list es) :: is in

    let rec loop ds =
      match ds with
        | [] -> []
        | (_, vars, id, t)::ds as ds' ->
            <:binding@g<
              $lid:G.xdr id$ =
              $List.fold_right
                (fun v e -> <:expr@g< fun $lid:G.xdr_p v$ -> $e$ >>)
                vars
                (let bid = id_digest id (List.map (fun v -> Var (g, v)) vars) in
                 (* ds' includes current def for polymorphic instantiation *)
                 gen_xdr_def [] [] ds' bid t)$
            >> :: loop ds in
    let is = List.map (fun b -> StVal (g, BFalse, b)) (loop ds) @ is in

    match name with
      | None ->
          let ts =
            List.map
              (fun (_, vars, id, t) ->
                let vars = List.map (fun v -> TyQuo (g, v)) vars in
                TyDcl (g, id, vars, G.gen_type t, []))
              ds in
          StTyp (g, tyAnd_of_list ts) :: is

      | Some _ -> is in

  let gen_func ?name (_, id, args, res) is =
    let arg =
      match args with
        | [] -> assert false
        | [a] -> a
        | _ -> Tuple (g, args) in
    let items aid arg is =
      <:str_item@g<
        type $lid:aid$ = $G.gen_type ?name arg$
        let $lid:G.to_ aid$ x = $gen_to ?name arg <:expr@g< x >>$
        let $lid:G.of_ aid$ v = $gen_of ?name arg <:expr@g< v >>$
        let $lid:G.xdr aid$ = $gen_xdr [] [] [] arg$
      >> :: is in
    let is = items (G.res id) res is in
    let is = items (G.arg id) arg is in
    let is =
      List.foldi_right
        (fun arg i is -> <:str_item@g< type $lid:G.argi id i$ = $G.gen_type ?name arg$ >> :: is)
        args
        is in
    is in

  let program funcs =
    <:str_item@g<
      let program =
        Rpc_program.create
          (Rtypes.uint4_of_int 0)
          (Rtypes.uint4_of_int 0)
          (Xdr.validate_xdr_type_system [])
          [ $exSem_of_list
              (List.mapi
                  (fun (_,id,_,_) i ->
                    <:expr@g<
                      $`str:id$,
                      (Rtypes.uint4_of_int $`int:i$,
                      $lid:G.xdr_arg id$,
                      $lid:G.xdr_res id$)
                    >>)
                  funcs)$ ]
        >> in

  match intf with
    | Simple (typedefs, funcs) ->
        stSem_of_list
          (List.fold_right gen_typedef typedefs
              (List.fold_right gen_func funcs
                  [ program funcs ]))

    | Modules (typedefs, (_, _, funcs), _) ->
        stSem_of_list
          (List.fold_right (gen_typedef ~name) typedefs
              (List.fold_right (gen_func ~name) funcs
                  [ program funcs ]))
