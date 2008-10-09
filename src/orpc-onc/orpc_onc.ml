(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008 Skydeck, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA
 *)

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

let to_orpc_result to'a to'b x =
  match Xdr.dest_xv_union_over_enum_fast x with
    | (0, x) -> Orpc.Orpc_success (to'a x)
    | (1, x) -> Orpc.Orpc_failure (to'b x)
    | _ -> assert false

let of_orpc_result of'a of'b x =
  match x with
    | Orpc.Orpc_success x -> Xdr.XV_union_over_enum_fast (0, of'a x)
    | Orpc.Orpc_failure x -> Xdr.XV_union_over_enum_fast (1, of'b x)

let xdr_orpc_result xdr'a xdr'b =
  Xdr.X_rec ("orpc_result",
            Xdr.X_union_over_enum
              (Xdr.X_enum [ ("Orpc_success", (Rtypes.int4_of_int 0));
                            ("Orpc_failure", (Rtypes.int4_of_int 1)) ],
              [ ("Orpc_success", xdr'a); ("Orpc_failure", xdr'b) ], None))

let session = ref None
