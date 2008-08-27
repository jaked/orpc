(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008 Skydeck, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
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

let pp_array pp'a fmt v =
  Format.fprintf fmt "@[<hv 3>[| %a |]@]"
    (fun fmt v ->
      let len = Array.length v in
      for i = 0 to len - 1
      do
        pp'a fmt v.(i);
        if i < len - 1
        then Format.fprintf fmt ";@ "
      done)
    v

let pp_list pp'a fmt v =
  Format.fprintf fmt "@[<hv 2>[ %a ]@]"
    (fun fmt v ->
      let rec loop v =
        match v with
          | [] -> ()
          | [v] -> pp'a fmt v
          | v::vs ->
              Format.fprintf fmt "%a;@ " pp'a v;
              loop vs in
      loop v)
    v

let pp_option pp'a fmt v =
  match v with
    | None -> Format.fprintf fmt "None"
    | Some v -> Format.fprintf fmt "@[<hv 1>(Some@ %a)@]" pp'a v

let session = ref None

module type Trace =
sig
  type t
  val trace_call : string -> (Format.formatter -> unit) -> t
  val trace_reply_ok : t -> (Format.formatter -> unit) -> unit
  val trace_reply_exn : t -> exn -> (Format.formatter -> unit) -> unit
end

module Trace_of_formatter (F : sig val formatter : Format.formatter end) : Trace =
struct
  type t = unit
  let trace_call _ f = f F.formatter
  let trace_reply_ok _ f = f F.formatter
  let trace_reply_exn _ _ f = f F.formatter
end
