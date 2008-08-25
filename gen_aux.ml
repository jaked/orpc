open Camlp4.PreCast
open Ast
open Types
open Util

let _loc = Camlp4.PreCast.Loc.ghost

module G = Gen_common

let gen_aux_mli name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

  let gen_typedef_typs ds =
    SgTyp (_loc,
          tyAnd_of_list
            (List.map
                (fun (_, vars, id, t) ->
                  TyDcl (_loc, id, G.tvars vars, G.gen_type qual_id t, []))
                ds)) in

  let gen_typedef_funs ds =
    sgSem_of_list
      (List.map
          (fun (_, vars, id, _) ->
            let appd =
              G.tapps <:ctyp< $id:qual_id id$ >> (G.tvars vars) in

            <:sig_item<
              val $lid:G.to_ id$ :
                $G.arrows
                  (List.map (fun v -> <:ctyp< Xdr.xdr_value -> '$lid:v$ >>) vars)
                  <:ctyp< Xdr.xdr_value -> $appd$ >>$

              val $lid:G.of_ id$ :
                $G.arrows
                  (List.map (fun v -> <:ctyp< '$lid:v$ -> Xdr.xdr_value >>) vars)
                  <:ctyp< $appd$ -> Xdr.xdr_value >>$

              val $lid:G.xdr id$ :
                $G.arrows
                  (List.map (fun v -> <:ctyp< Xdr.xdr_type_term >>) vars)
                  <:ctyp< Xdr.xdr_type_term >>$
          >>)
          ds) in

  let gen_exc (_, id, ts) =
    <:sig_item<
      exception $uid:id$ of $tyAnd_of_list (List.map (G.gen_type qual_id) ts)$
    >> in

  let gen_func (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (g, args) in
    let orpc_res =
      if has_excs
      then Apply (_loc, Some "Orpc", "orpc_result", [res; Apply (_loc, None, "exn", [])])
      else res in
    let items aid arg =
      let t = G.gen_type qual_id arg in
      <:sig_item<
        val $lid:G.to_ aid$ : Xdr.xdr_value -> $t$
        val $lid:G.of_ aid$ : $t$ -> Xdr.xdr_value
        val $lid:G.xdr aid$ : Xdr.xdr_type_term
      >> in
    <:sig_item<
      $items (G.res0 id) res$ ;;
      $items (G.arg id) arg$
      $items (G.res id) orpc_res$
    >> in

  <:sig_item<
    $match mode with
      | Simple ->
          <:sig_item<
            $sgSem_of_list (List.map gen_typedef_typs typedefs)$ ;;
            $sgSem_of_list (List.map gen_exc excs)$ ;;
          >>
      | _ -> <:sig_item< >>$ ;;
    $sgSem_of_list (List.map gen_typedef_funs typedefs)$ ;;
    $if has_excs
     then
       <:sig_item<
         val to_exn : Xdr.xdr_value -> exn
         val of_exn : exn -> Xdr.xdr_value
         val xdr_exn : Xdr.xdr_type_term ;;
       >>
     else <:sig_item< >>$ ;;
    $sgSem_of_list (List.map gen_func funcs)$ ;;
    val program : Rpc_program.t
  >>

let gen_aux_ml name (typedefs, excs, funcs, mode) =

  let has_excs = excs <> [] in
  let qual_id = G.qual_id name mode in

  let rec gen_to t x =
    match t with
      | Var (_, id) -> <:expr< $lid:G.to_p id$ $x$ >>
      | Unit _ -> <:expr< () >>
      | Int _ -> <:expr< Rtypes.int_of_int4 (Xdr.dest_xv_int $x$) >>
      | Int32 _ -> <:expr< Rtypes.int32_of_int4 (Xdr.dest_xv_int $x$) >>
      | Int64 _ -> <:expr< Rtypes.int64_of_int8 (Xdr.dest_xv_hyper $x$) >>
      | Float _ -> <:expr< Rtypes.float_of_fp8 (Xdr.dest_xv_double $x$) >>
      | Bool _ -> <:expr< Xdr.dest_xv_enum_fast $x$ = 1 >>
      | Char _ -> <:expr< char_of_int (Xdr.dest_xv_enum_fast $x$) >>
      | String _ -> <:expr< Xdr.dest_xv_string $x$>>
  
      | Tuple (_, parts) ->
          let (pps, pes) = G.vars parts in
          <:expr<
            match Xdr.dest_xv_struct_fast $x$ with
              | [| $paSem_of_list pps$ |] -> ( $exCom_of_list (List.map2 gen_to parts pes)$ )
              | _ -> assert false
          >>
  
      | Record (_, fields) ->
          let (fps, fes) = G.vars fields in
          let rb f e = <:rec_binding< $id:qual_id f.f_id$ = $gen_to f.f_typ e$ >> in
          <:expr<
            match Xdr.dest_xv_struct_fast $x$ with
              | [| $paSem_of_list fps$ |] ->
                  $ExRec(_loc, rbSem_of_list (List.map2 rb fields fes), <:expr< >>)$
              | _ -> assert false
          >>
  
       | Variant (_, arms) ->
           let mc (id, ts) i =
             match ts with
               | [] -> <:match_case< ($`int:i$, _) -> $id:qual_id id$ >>
               | [t] -> <:match_case< ($`int:i$, x) -> $id:qual_id id$ $gen_to t <:expr< x >>$ >>
               | _ ->
                   let (pps, pes) = G.vars ts in
                   <:match_case<
                     ($`int:i$, x) ->
                       match Xdr.dest_xv_struct_fast x with
                         | [| $paSem_of_list pps$ |] ->
                             $List.fold_left
                               (fun ps p -> <:expr< $ps$ $p$ >>)
                               <:expr< $id:qual_id id$ >>
                               (List.map2 gen_to ts pes)$
                         | _ -> assert false
                   >> in
           ExMat (_loc, <:expr< Xdr.dest_xv_union_over_enum_fast $x$ >>,
                 mcOr_of_list (List.mapi mc arms @ [ <:match_case< _ -> assert false >> ]))
  
       | Array (_, t) ->
           <:expr< Array.map (fun x -> $gen_to t <:expr< x >>$) (Xdr.dest_xv_array $x$) >>
  
       | List (_, t) ->
           <:expr< Orpc.to_list (fun x -> $gen_to t <:expr< x >>$) $x$ >>
  
       | Option (_, t) ->
           <:expr< Orpc.to_option (fun x -> $gen_to t <:expr< x >>$) $x$ >>
  
       | Apply (_, mdl, id, args) ->
           <:expr<
             $G.apps
               (match mdl with
                 | None -> <:expr< $lid:G.to_ id$ >>
                 | Some mdl -> <:expr< $uid:mdl$ . $lid:G.to_ id$ >>)
               (List.map (fun a -> <:expr< fun x -> $gen_to a <:expr< x >>$ >>) args)$
             $x$
           >>
  
       | Arrow _ -> assert false in

  let rec gen_of t v =
    match t with
      | Var (_, id) -> <:expr< $lid:G.of_p id$ $v$ >>
      | Unit _ -> <:expr< Xdr.XV_void >>
      | Int _ -> <:expr< Xdr.XV_int (Rtypes.int4_of_int $v$) >>
      | Int32 _ -> <:expr< Xdr.XV_int (Rtypes.int4_of_int32 $v$) >>
      | Int64 _ -> <:expr< Xdr.XV_hyper (Rtypes.int8_of_int64 $v$) >>
      | Float _ -> <:expr< Xdr.XV_double (Rtypes.fp8_of_float $v$) >>
      | Bool _ -> <:expr< Xdr.XV_enum_fast (if $v$ then 1 else 0) >>
      | Char _ -> <:expr< Xdr.XV_enum_fast (int_of_char $v$) >>
      | String _ -> <:expr< Xdr.XV_string $v$ >>
  
      | Tuple (_, parts) ->
          let (pps, pes) = G.vars parts in
          <:expr<
            let ( $paCom_of_list pps$ ) = $v$ in
            Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of parts pes)$ |]
          >>
  
      | Record (_, fields) ->
          let (fps, fes) = G.vars fields in
          let rb f p = <:patt< $id:qual_id f.f_id$ = $p$ >> in
          <:expr<
            let { $paSem_of_list (List.map2 rb fields fps)$ } = $v$ in
            Xdr.XV_struct_fast
              [| $exSem_of_list (List.map2 (fun f v -> gen_of f.f_typ v) fields fes)$ |]
          >>
  
       | Variant (_, arms) ->
           let mc (id, ts) i =
             match ts with
               | [] ->
                   <:match_case<
                     $id:qual_id id$ ->
                       Xdr.XV_union_over_enum_fast ($`int:i$, Xdr.XV_void)
                   >>
               | [t] ->
                   <:match_case<
                     $id:qual_id id$ x ->
                       Xdr.XV_union_over_enum_fast ($`int:i$, $gen_of t <:expr< x >>$)
                   >>
               | _ ->
                   let (pps, pes) = G.vars ts in
                   <:match_case<
                     $id:qual_id id$ ( $paCom_of_list pps$ ) ->
                       Xdr.XV_union_over_enum_fast
                         ($`int:i$,
                         Xdr.XV_struct_fast [| $exSem_of_list (List.map2 gen_of ts pes)$ |])
                   >> in
           ExMat (_loc, <:expr< $v$ >>,
                 mcOr_of_list (List.mapi mc arms))
  
       | Array (_, t) ->
           <:expr< Xdr.XV_array (Array.map (fun v -> $gen_of t <:expr< v >>$) $v$) >>
  
       | List (_, t) ->
           <:expr< Orpc.of_list (fun v -> $gen_of t <:expr< v >>$) $v$ >>
  
       | Option (_, t) ->
           <:expr< Orpc.of_option (fun v -> $gen_of t <:expr< v >>$) $v$ >>
  
       | Apply (_, mdl, id, args) ->
           <:expr<
             $G.apps
               (match mdl with
                 | None -> <:expr< $lid:G.of_ id$ >>
                 | Some mdl -> <:expr< $uid:mdl$ . $lid:G.of_ id$ >>)
               (List.map (fun a -> <:expr< fun v -> $gen_of a <:expr< v >>$ >>) args)$
             $v$
           >>
  
       | Arrow _ -> assert false in

  let gen_of_exc t v =
    match gen_of t v with
      | ExMat (loc, e, cases) ->
          ExMat (loc, e, McOr(_loc, cases, <:match_case< _ -> raise $v$ >>))
      | _ -> assert false in

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
            with Not_found -> <:expr< $lid:G.xdr_p id$ >>
          end
  
      | Unit _ -> <:expr< Xdr.X_void >>
      | Int _ -> <:expr< Xdr.X_int >>
      | Int32 _ -> <:expr< Xdr.X_int >>
      | Int64 _ -> <:expr< Xdr.X_hyper >>
      | Float _ -> <:expr< Xdr.X_double >>
      | Bool _ -> <:expr< Xdr.x_bool >>
      | Char _ -> <:expr< Orpc.x_char >>
      | String _ -> <:expr< Xdr.x_string_max >>
  
      | Tuple (_, parts) ->
          let px t i = <:expr< ( $`str:string_of_int i$, $gx t$ ) >> in
          <:expr< Xdr.X_struct [ $exSem_of_list (List.mapi px parts)$ ] >>
  
      | Record (_, fields) ->
          let fx f = <:expr< ( $`str:f.f_id$, $gx f.f_typ$ ) >> in
          <:expr< Xdr.X_struct [ $exSem_of_list (List.map fx fields)$ ] >>
  
      | Variant (_, arms) ->
          let tag (id, _) i = <:expr< ( $`str:id$, Rtypes.int4_of_int $`int:i$ ) >> in
          let ax (id, ts) =
            match ts with
              | [] -> <:expr< ( $`str:id$, Xdr.X_void ) >>
              | [t] -> <:expr< ( $`str:id$,  $gx t$ ) >>
              | _ ->
                  let px t i = <:expr< ( $`str:string_of_int i$, $gx t$ ) >> in
                  <:expr< ( $`str:id$, Xdr.X_struct [ $exSem_of_list (List.mapi px ts)$ ]) >> in
          <:expr<
            Xdr.X_union_over_enum
              (Xdr.X_enum [ $exSem_of_list (List.mapi tag arms)$ ],
              [ $exSem_of_list (List.map ax arms)$ ],
              None)
          >>
  
      | Array (_, t) -> <:expr< Xdr.x_array_max $gx t$ >>
  
      | List (_, t) -> <:expr< Orpc.x_list $gx t$ >>
  
      | Option (_, t) -> <:expr< Xdr.x_optional $gx t$ >>
  
      | Apply (_, mdl, id, args) ->
          (* XXX should check that mdl = None for these first two cases *)
  
          if List.mem id bs
          (* refer to a def in scope *)
          then <:expr< Xdr.X_refer $`str:id$ >>
  
          else begin
            try
              let (_, vars, _, t) = List.find (fun (_,_,id',_) -> id' = id) ds in
              (* inline / instantiate a forward def. can just replace vs because defs have no free variables. *)
              gen_xdr_def (List.combine vars (List.map gx args)) bs ds id t
  
            with Not_found ->
              (* refer to a previous def at the OCaml level *)
              G.apps
               (match mdl with
                 | None -> <:expr< $lid:G.xdr id$ >>
                 | Some mdl -> <:expr< $uid:mdl$ . $lid:G.xdr id$ >>)
                (List.map gx args)
          end
  
       | Arrow _ -> assert false

  and gen_xdr_def vs bs ds id t =
    <:expr< Xdr.X_rec ($`str:id$, $gen_xdr vs (id::bs) ds t$) >> in

  let gen_typedef_typs ds =
    StTyp (_loc,
          tyAnd_of_list
            (List.map
                (fun (_, vars, id, t) ->
                  let vars = List.map (fun v -> <:ctyp< '$lid:v$ >>) vars in
                  TyDcl (_loc, id, vars, G.gen_type qual_id t, []))
                ds)) in

  let gen_typedef_funs ds =
    <:str_item<
      $let es =
         List.map
           (fun (_, vars, id, t) ->
             <:binding<
               $lid:G.to_ id$ =
               $G.funs_ids
                 (List.map G.to_p vars)
                 <:expr< fun x -> $gen_to t <:expr< x >>$ >>$
             >>)
           ds in
       StVal (_loc, BTrue, biAnd_of_list es)$ ;;

      $let es =
         List.map
           (fun (_, vars, id, t) ->
             <:binding<
               $lid:G.of_ id$ =
               $G.funs_ids
                 (List.map G.of_p vars)
                 <:expr< fun x -> $gen_of t <:expr< x >>$ >>$
             >>)
           ds in
       StVal (_loc, BTrue, biAnd_of_list es)$ ;;

      $let rec loop ds =
         match ds with
           | [] -> []
           | (_, vars, id, t)::ds ->
               <:binding<
                 $lid:G.xdr id$ =
                 $G.funs_ids
                   (List.map G.xdr_p vars)
                   (gen_xdr_def [] [] ds id t)$
               >> :: loop ds in
       stSem_of_list (List.map (fun b -> StVal (_loc, BFalse, b)) (loop ds))$ ;;
    >> in

  let gen_exc (_, id, ts) =
    <:str_item<
      exception $uid:id$ of $tyAnd_of_list (List.map (G.gen_type qual_id) ts)$
    >> in

  let gen_func (_, id, args, res) =
    let arg =
      match List.map typ_of_argtyp_option args with
        | [] -> assert false
        | [a] -> a
        | args -> Tuple (_loc, args) in
    let orpc_res =
      if has_excs
      then Apply (_loc, Some "Orpc", "orpc_result", [res; Apply (_loc, None, "exn", [])])
      else res in
    let items aid arg =
      <:str_item<
        let $lid:G.to_ aid$ x = $gen_to arg <:expr< x >>$
        let $lid:G.of_ aid$ v = $gen_of arg <:expr< v >>$
        let $lid:G.xdr aid$ = $gen_xdr [] [] [] arg$
      >> in
    <:str_item<
      $items (G.res0 id) res$ ;;
      $items (G.arg id) arg$ ;;
      $items (G.res id) orpc_res$ ;;
    >> in

  <:str_item<
    $match mode with
      | Simple ->
          <:str_item<
            $stSem_of_list (List.map gen_typedef_typs typedefs)$ ;;
            $stSem_of_list (List.map gen_exc excs)$ ;;
          >>
      | _ -> <:str_item< >>$ ;;
    $stSem_of_list (List.map gen_typedef_funs typedefs)$ ;;
    $if has_excs
     then
       let t = Variant (_loc, List.map (fun (_, id, ts) -> (id, ts)) excs) in
       <:str_item<
         let to_exn x = $gen_to t <:expr< x >>$
         let of_exn v = $gen_of_exc t <:expr< v >>$
         let xdr_exn = $gen_xdr [] [] [] t$ ;;
       >>
     else <:str_item< >>$ ;;
    $stSem_of_list (List.map gen_func funcs)$ ;;

    let program =
      Rpc_program.create
        (Rtypes.uint4_of_int 0)
        (Rtypes.uint4_of_int 0)
        (Xdr.validate_xdr_type_system [])
        [ $exSem_of_list
            (List.mapi
                (fun (_,id,_,_) i ->
                  <:expr<
                    $`str:id$,
                    (Rtypes.uint4_of_int $`int:i$,
                    $lid:G.xdr_arg id$,
                    $lid:G.xdr_res id$)
                  >>)
                funcs)$ ]
  >>
