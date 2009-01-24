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
