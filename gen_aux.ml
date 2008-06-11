open Camlp4.PreCast
open Ast
open S_ast
open Util

let g = Camlp4.PreCast.Loc.ghost

module G = Gen_common

let orpc_result =
  g,
  ["a", "b"],
  "orpc_result",
  Variant (g, ["Orpc_success", [Var (g, "a")];
               "Orpc_failure", [Var (g, "b")]])

let typ_of_argtyp_option = function
  | Unlabelled (_, t) -> t
  | Labelled (_, _, t) -> t
  | Optional (loc, _, t) -> Option (loc, t)

let gen_aux_mli name (typedefs, excs, funcs, kinds) =
  let gen_typedef ?name ds =
    let is =
      List.map
        (fun (_, vars, id, _) ->
          let appd =
            let t =
              match name, id with
                | _, "exn" (* hack *)
                | None, _ -> <:ctyp@g< $lid:id$ >>
                | Some name, _ -> <:ctyp@g< $uid:name$.$lid:id$ >> in
            G.tapps t (List.map (fun v -> <:ctyp@g< '$lid:v$ >>) vars) in

          <:sig_item@g<
            val $lid:G.to_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp@g< Xdr.xdr_value -> '$lid:v$ >>) vars)
                <:ctyp@g< Xdr.xdr_value -> $appd$ >>$

            val $lid:G.of_ id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp@g< '$lid:v$ -> Xdr.xdr_value >>) vars)
                <:ctyp@g< $appd$ -> Xdr.xdr_value >>$

            val $lid:G.xdr id$ :
              $G.arrows
                (List.map (fun v -> <:ctyp@g< Xdr.xdr_type_term >>) vars)
                <:ctyp@g< Xdr.xdr_type_term >>$
          >>)
        ds in

    <:sig_item@g<
      $match name with
        | None ->
            let ts =
              List.map
                (fun (_, vars, id, t) ->
                  let vars = List.map (fun v -> TyQuo (g, v)) vars in
                  TyDcl (g, id, vars, G.gen_type t, []))
                ds in
            SgTyp (g, tyAnd_of_list ts)
        | Some _ -> SgNil g$ ;;
      $sgSem_of_list is$
    >> in

  let gen_exc ?name (_, id, ts) =
    match name with
      | None ->
          <:sig_item@g<
            exception $uid:id$ of $tyAnd_of_list (List.map G.gen_type ts)$
          >>
      | Some _ -> SgNil g in

  let gen_func ~has_excs ?name (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (g, args) in
    let orpc_res =
      if has_excs
      then Apply (g, Some "Orpc", "orpc_result", [res; Apply (g, None, "exn", [])])
      else res in
    let items aid arg =
      <:sig_item@g<
        type $lid:aid$ = $G.gen_type ?name arg$
        val $lid:G.to_ aid$ : Xdr.xdr_value -> $lid:aid$
        val $lid:G.of_ aid$ : $lid:aid$ -> Xdr.xdr_value
        val $lid:G.xdr aid$ : Xdr.xdr_type_term
      >> in
    <:sig_item@g<
      $items (G.res0 id) res$ ;;
      $items (G.arg id) arg$
      $items (G.res id) orpc_res$
      $sgSem_of_list
        (List.mapi
            (fun arg i -> <:sig_item@g< type $lid:G.argi id i$ = $G.gen_type ?name arg$ >>)
            (List.map typ_of_argtyp args))$
    >> in

  let name = match kinds with [] -> None | _ -> Some name in
  let has_excs = excs <> [] in
  <:sig_item@g<
    $sgSem_of_list (List.map (gen_typedef ?name) typedefs)$ ;;
    $sgSem_of_list (List.map (gen_exc ?name) excs)$ ;;
    $if has_excs
     then
       <:sig_item@g<
         val to_exn : Xdr.xdr_value -> exn
         val of_exn : exn -> Xdr.xdr_value
         val xdr_exn : Xdr.xdr_type_term ;;
       >>
     else SgNil g$ ;;
    $sgSem_of_list (List.map (gen_func ~has_excs ?name) funcs)$ ;;
    val program : Rpc_program.t
  >>

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
    | String _ -> <:expr@g< Xdr.dest_xv_string $x$>>

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
           let con =
             match name with
               | None -> <:expr@g< $uid:id$ >>
               | Some name -> <:expr@g< $uid:name$.$uid:id$ >> in
           match ts with
             | [] -> <:match_case@g< ($`int:i$, _) -> $con$ >>
             | [t] -> <:match_case@g< ($`int:i$, x) -> $con$ $gen_to t <:expr@g< x >>$ >>
             | _ ->
                 let (pps, pes) = G.vars ts in
                 <:match_case@g<
                   ($`int:i$, x) ->
                     match Xdr.dest_xv_struct_fast x with
                       | [| $paSem_of_list pps$ |] ->
                           $List.fold_left
                             (fun ps p -> <:expr@g< $ps$ $p$ >>)
                             con
                             (List.map2 gen_to ts pes)$
                       | _ -> assert false
                 >> in
         ExMat (g, <:expr@g< Xdr.dest_xv_union_over_enum_fast $x$ >>,
               mcOr_of_list (List.mapi mc arms @ [ <:match_case@g< _ -> assert false >> ]))

     | Array (_, t) ->
         <:expr@g< Array.map (fun x -> $gen_to t <:expr@g< x >>$) (Xdr.dest_xv_array $x$) >>

     | List (_, t) ->
         <:expr@g< Orpc.to_list (fun x -> $gen_to t <:expr@g< x >>$) $x$ >>

     | Option (_, t) ->
         <:expr@g< Orpc.to_option (fun x -> $gen_to t <:expr@g< x >>$) $x$ >>

     | Apply (_, mdl, id, args) ->
         <:expr@g<
           $G.apps
             (match mdl with
               | None -> <:expr@g< $lid:G.to_ id$ >>
               | Some mdl -> <:expr@g< $uid:mdl$ . $lid:G.to_ id$ >>)
             (List.map (fun a -> <:expr@g< fun x -> $gen_to a <:expr@g< x >>$ >>) args)$
           $x$
         >>

     | Arrow _ -> assert false

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
           (* I don't see how to substitute just the constructor into these patterns *)
           match ts with
             | [] ->
                 let conp =
                   match name with
                     | None -> <:patt@g< $uid:id$ >>
                     | Some name -> <:patt@g< $uid:name$.$uid:id$ >> in
                 <:match_case@g< $conp$ -> Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void) >>
             | [t] ->
                 let conp =
                   match name with
                     | None -> <:patt@g< $uid:id$ x >>
                     | Some name -> <:patt@g< $uid:name$.$uid:id$ x >> in
                 <:match_case@g< $conp$ -> Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr@g< x >>$) >>
             | _ ->
                 let (pps, pes) = G.vars ts in
                 let conp =
                   match name with
                     | None -> <:patt@g< $uid:id$ ( $paCom_of_list pps$ ) >>
                     | Some name -> <:patt@g< $uid:name$.$uid:id$  ( $paCom_of_list pps$ ) >> in
                 <:match_case@g<
                   $conp$ ->
                     Xdr.XV_union_over_enum_fast
                       ($`int:i$,
                       Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                 >> in
         ExMat (g, <:expr@g< $v$ >>,
               mcOr_of_list (List.mapi mc arms))

     | Array (_, t) ->
         <:expr@g< Xdr.XV_array (Array.map (fun v -> $gen_of t <:expr@g< v >>$) $v$) >>

     | List (_, t) ->
         <:expr@g< Orpc.of_list (fun v -> $gen_of t <:expr@g< v >>$) $v$ >>

     | Option (_, t) ->
         <:expr@g< Orpc.of_option (fun v -> $gen_of t <:expr@g< v >>$) $v$ >>

     | Apply (_, mdl, id, args) ->
         <:expr@g<
           $G.apps
             (match mdl with
               | None -> <:expr@g< $lid:G.of_ id$ >>
               | Some mdl -> <:expr@g< $uid:mdl$ . $lid:G.of_ id$ >>)
             (List.map (fun a -> <:expr@g< fun v -> $gen_of a <:expr@g< v >>$ >>) args)$
           $v$
         >>

     | Arrow _ -> assert false

let gen_of_exc ?name t v =
  match gen_of ?name t v with
    | ExMat (loc, e, cases) ->
        ExMat (loc, e, McOr(g, cases, <:match_case@g< _ -> raise $v$ >>))
    | _ -> assert false

(*
  this is kind of hairy because Xdr.xdr_type_term doesn't have a nice
  way to express mutual recursion and polymorphism. the basic problem
  is that while X_rec / X_refer give you a way to tie a single
  recursive loop, for forward references in mutual recursion you have
  to inline a copy of the term you're referring to. polymorphism
  complicates things: ordinarily we express it at the OCaml level, but
  when we inline a term we have to instantiate it explicitly.

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
    | Char _ -> <:expr@g< Orpc.x_char >>
    | String _ -> <:expr@g< Xdr.x_string_max >>

    | Tuple (_, parts) ->
        let px t i = <:expr@g< ( $`str:string_of_int i$, $gx t$ ) >> in
        <:expr@g< Xdr.X_struct [ $exSem_of_list (List.mapi px parts)$ ] >>

    | Record (_, fields) ->
        let fx (id, t) = <:expr@g< ( $`str:id$, $gx t$ ) >> in
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

    | Array (_, t) -> <:expr@g< Xdr.x_array_max $gx t$ >>

    | List (_, t) -> <:expr@g< Orpc.x_list $gx t$ >>

    | Option (_, t) -> <:expr@g< Xdr.x_optional $gx t$ >>

    | Apply (_, mdl, id, args) ->
        (* XXX should check that mdl = None for these first two cases *)

        if List.mem id bs
        (* refer to a def in scope *)
        then <:expr@g< Xdr.X_refer $`str:id$ >>

        else begin
          try
            let (_, vars, _, t) = List.find (fun (_,_,id',_) -> id' = id) ds in
            (* inline / instantiate a forward def. can just replace vs because defs have no free variables. *)
            gen_xdr_def (List.combine vars (List.map gx args)) bs ds id t

          with Not_found ->
            (* refer to a previous def at the OCaml level *)
            G.apps
             (match mdl with
               | None -> <:expr@g< $lid:G.xdr id$ >>
               | Some mdl -> <:expr@g< $uid:mdl$ . $lid:G.xdr id$ >>)
              (List.map gx args)
        end

     | Arrow _ -> assert false

and gen_xdr_def vs bs ds id t =
  <:expr@g< Xdr.X_rec ($`str:id$, $gen_xdr vs (id::bs) ds t$) >>

let gen_aux_ml name (typedefs, excs, funcs, kinds) =
  let gen_typedef ?name ds =
    <:str_item@g<
      $match name with
        | None ->
            let ts =
              List.map
                (fun (_, vars, id, t) ->
                  let vars = List.map (fun v -> TyQuo (g, v)) vars in
                  TyDcl (g, id, vars, G.gen_type t, []))
                ds in
            StTyp (g, tyAnd_of_list ts)

        | Some _ -> StNil g$;;

      $let es =
         List.map
           (fun (_, vars, id, t) ->
             <:binding@g<
               $lid:G.to_ id$ =
               $G.funs_ids
                 (List.map G.to_p vars)
                 <:expr@g< fun x -> $gen_to ?name t <:expr@g< x >>$ >>$
             >>)
           ds in
       StVal (g, BTrue, biAnd_of_list es)$ ;;

      $let es =
         List.map
           (fun (_, vars, id, t) ->
             <:binding@g<
               $lid:G.of_ id$ =
               $G.funs_ids
                 (List.map G.of_p vars)
                 <:expr@g< fun x -> $gen_of ?name t <:expr@g< x >>$ >>$
             >>)
           ds in
       StVal (g, BTrue, biAnd_of_list es)$ ;;

      $let rec loop ds =
         match ds with
           | [] -> []
           | (_, vars, id, t)::ds ->
               <:binding@g<
                 $lid:G.xdr id$ =
                 $G.funs_ids
                   (List.map G.xdr_p vars)
                   (gen_xdr_def [] [] ds id t)$
               >> :: loop ds in
       stSem_of_list (List.map (fun b -> StVal (g, BFalse, b)) (loop ds))$ ;;
    >> in

  let gen_exc ?name (_, id, ts) =
    match name with
      | None ->
          <:str_item@g<
            exception $uid:id$ of $tyAnd_of_list (List.map G.gen_type ts)$
          >>
      | Some _ -> StNil g in

  let gen_func ~has_excs ?name (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (g, args) in
    let orpc_res =
      if has_excs
      then Apply (g, Some "Orpc", "orpc_result", [res; Apply (g, None, "exn", [])])
      else res in
    let items aid arg =
      <:str_item@g<
        type $lid:aid$ = $G.gen_type ?name arg$
        let $lid:G.to_ aid$ x = $gen_to ?name arg <:expr@g< x >>$
        let $lid:G.of_ aid$ v = $gen_of ?name arg <:expr@g< v >>$
        let $lid:G.xdr aid$ = $gen_xdr [] [] [] arg$
      >> in
    <:str_item@g<
      $items (G.res0 id) res$ ;;
      $items (G.arg id) arg$ ;;
      $items (G.res id) orpc_res$ ;;
      $stSem_of_list
        (List.mapi
            (fun arg i -> <:str_item@g< type $lid:G.argi id i$ = $G.gen_type ?name arg$ >>)
            (List.map typ_of_argtyp args))$
    >> in

  let name = match kinds with [] -> None | _ -> Some name in
  let has_excs = excs <> [] in
  <:str_item@g<
    $stSem_of_list (List.map (gen_typedef ?name) typedefs)$ ;;
    $stSem_of_list (List.map (gen_exc ?name) excs)$ ;;
    $if has_excs
     then
       let t = Variant (g, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item@g<
         let to_exn x = $gen_to ?name t <:expr@g< x >>$
         let of_exn v = $gen_of_exc ?name t <:expr@g< v >>$
         let xdr_exn = $gen_xdr [] [] [] t$ ;;
       >>
     else StNil g$ ;;
    $stSem_of_list (List.map (gen_func ~has_excs ?name) funcs)$ ;;

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
  >>
