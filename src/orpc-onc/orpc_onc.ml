(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Copyright (C) 2010 Jacob Donham
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

let x_list x'a = Xdr.x_array_max x'a

let to_list to'a x =
  let a = Xdr.dest_xv_array x in
  let rec tolist i res = (* following Array.to_list *)
    if i < 0 then res else tolist (i - 1) (to'a (Array.unsafe_get a i) :: res) in
  tolist (Array.length a - 1) []

let of_list of'a = function (* following Array.of_list *)
  | [] -> Xdr.XV_array [||]
  | hd::tl as l ->
      let a = Array.create (List.length l) (of'a hd) in
      let rec fill i = function
        | [] -> a
        | hd::tl -> Array.unsafe_set a i (of'a hd); fill (i+1) tl in
      Xdr.XV_array (fill 1 tl)

let to_option to'a x =
  match Xdr.dest_xv_union_over_enum_fast x with
    | (0, _) -> None
    | (1, x) -> Some (to'a x)
    | _ -> assert false

let of_option of'a v =
  match v with
    | None -> Xdr.XV_union_over_enum_fast (0, Xdr.XV_void)
    | Some v -> Xdr.XV_union_over_enum_fast (1, of'a v)

let orpc_aux_to_orpc_result to'a to'b x =
  match Xdr.dest_xv_union_over_enum_fast x with
    | (0, x) -> Orpc.Orpc_success (to'a x)
    | (1, x) -> Orpc.Orpc_failure (to'b x)
    | _ -> assert false

let orpc_aux_of_orpc_result of'a of'b x =
  match x with
    | Orpc.Orpc_success x -> Xdr.XV_union_over_enum_fast (0, of'a x)
    | Orpc.Orpc_failure x -> Xdr.XV_union_over_enum_fast (1, of'b x)

let orpc_aux_xdr_orpc_result xdr'a xdr'b =
  Xdr.X_rec ("orpc_result",
            Xdr.X_union_over_enum
              (Xdr.X_enum [ ("Orpc_success", (Rtypes.int4_of_int 0));
                            ("Orpc_failure", (Rtypes.int4_of_int 1)) ],
              [ ("Orpc_success", xdr'a); ("Orpc_failure", xdr'b) ], None))

let session = ref None
