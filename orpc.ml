let x_char =
  Xdr.X_enum (let rec loop n =
              if n = 256 then []
              else (String.make 1 (char_of_int n), Rtypes.int4_of_int n)::loop (n + 1) in
              loop 0)

let x_list x'a =
  Xdr.X_rec ("list",
            Xdr.X_union_over_enum
              (Xdr.x_bool,
              ["FALSE", Xdr.X_void;
               "TRUE", Xdr.X_struct ["0", x'a; "1", Xdr.X_refer "list"]],
              None))

let to_list to'a x =
  let rec loop x =
    match Xdr.dest_xv_union_over_enum_fast x with
      | (0, _) -> []
      | (1, x) ->
          (match Xdr.dest_xv_struct_fast x with
            | [| x0; x1 |] -> to'a x0 :: loop x1
            | _ -> assert false)
      | _ -> assert false in
  loop x

let of_list of'a v =
  let rec loop v =
    match v with
      | [] -> Xdr.XV_union_over_enum_fast (0, Xdr.XV_void)
      | v0::v1 -> Xdr.XV_union_over_enum_fast (1, Xdr.XV_struct_fast [| of'a v0; loop v1 |]) in
  loop v

let to_option to'a x =
  match Xdr.dest_xv_union_over_enum_fast x with
    | (0, _) -> None
    | (1, x) -> Some (to'a x)
    | _ -> assert false

let of_option of'a v =
  match v with
    | None -> Xdr.XV_union_over_enum_fast (0, Xdr.XV_void)
    | Some v -> Xdr.XV_union_over_enum_fast (1, of'a v)

(* 'b is always exn but the dummy param lets us pass in {of|to|xdr}_exn *)
type ('a, 'b) orpc_result = Orpc_success of 'a | Orpc_failure of exn

let to_orpc_result to'a to'b x =
  match Xdr.dest_xv_union_over_enum_fast x with
    | (0, x) -> Orpc_success (to'a x)
    | (1, x) -> Orpc_failure (to'b x)
    | _ -> assert false

let of_orpc_result of'a of'b x =
  match x with
    | Orpc_success x -> Xdr.XV_union_over_enum_fast (0, of'a x)
    | Orpc_failure x -> Xdr.XV_union_over_enum_fast (1, of'b x)

let xdr_orpc_result xdr'a xdr'b =
  Xdr.X_rec ("orpc_result",
            Xdr.X_union_over_enum
              (Xdr.X_enum [ ("Orpc_success", (Rtypes.int4_of_int 0));
                            ("Orpc_failure", (Rtypes.int4_of_int 1)) ],
              [ ("Orpc_success", xdr'a); ("Orpc_failure", xdr'b) ], None))

let pack_orpc_result f =
  try Orpc_success (f ())
  with e -> Orpc_failure e

let pack_orpc_result_async f k =
  try f (fun r -> k (Orpc_success r))
  with e -> k (Orpc_failure e)

let unpack_orpc_result v =
  match v with
    | Orpc_success v -> v
    | Orpc_failure e -> raise e

let format_array fmt'a fmt v =
  Format.fprintf fmt "@[<hv 3>[| %a |]@]"
    (fun fmt v ->
      let len = Array.length v in
      for i = 0 to len - 1
      do
        fmt'a fmt v.(i);
        if i < len - 1
        then Format.fprintf fmt ";@ "
      done)
    v

let format_list fmt'a fmt v =
  Format.fprintf fmt "@[<hv 2>[ %a ]@]"
    (fun fmt v ->
      let rec loop v =
        match v with
          | [] -> ()
          | [v] -> fmt'a fmt v
          | v::vs ->
              Format.fprintf fmt "%a;@ " fmt'a v;
              loop vs in
      loop v)
    v

let format_option fmt'a fmt v =
  match v with
    | None -> Format.fprintf fmt "None"
    | Some v -> Format.fprintf fmt "@[<hv 1>(Some@ %a)@]" fmt'a v
