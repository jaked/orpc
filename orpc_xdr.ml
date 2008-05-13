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
