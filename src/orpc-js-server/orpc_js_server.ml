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

let to_list to'a x =
  let rec loop x =
    if Obj.is_int x && Obj.obj x = 0 then []
    else if Obj.tag x = 0 && Obj.size x = 2 then to'a (Obj.field x 0) :: loop (Obj.field x 1)
    else raise (Invalid_argument "list") in
  loop x

let to_option to'a x =
  if Obj.is_int x && Obj.obj x = 0 then None
  else if Obj.tag x = 0 && Obj.size x = 1 then Some (to'a (Obj.field x 0))
  else raise (Invalid_argument "option")

(*
  XXX

  we use Obj.t here because in essence we are transferring the heap
  representation back and forth. unfortunately it is not identical
  between client and server: floats and int32s are unboxed on the
  Javascript side. we have to convert to boxed for regular OCaml, and
  we need types to guide the conversion, so that happens in the to_*
  functions in *_aux.ml.

  maybe in retrospect not such a clever idea.
*)

let serialize o =
  let module B = Buffer in
  let b = B.create 1024 in
  let rec loop o =
    let tag = Obj.tag o in
    if Obj.is_int o then B.add_string b (string_of_int (Obj.obj o))
    else if
      tag = Obj.lazy_tag || tag = Obj.closure_tag || tag = Obj.object_tag ||
      tag = Obj.infix_tag || tag = Obj.forward_tag || tag = Obj.no_scan_tag ||
      tag = Obj.abstract_tag || tag = Obj.custom_tag || tag = Obj.int_tag ||
      tag = Obj.out_of_heap_tag
    then
      raise (Invalid_argument "serialize: unserializeable heap object")
    else if tag = Obj.string_tag then (B.add_char b '"'; B.add_string b (Jslib_pp.escaped (Obj.obj o)); B.add_char b '"')
    else if tag = Obj.double_tag then B.add_string b (string_of_float (Obj.obj o)) (* XXX check float format *)
    else if tag = Obj.double_array_tag then raise (Invalid_argument "serialize: unimplemented")
    else
      let size = Obj.size o in
      if tag = 0
      then B.add_string b "$("
      else (B.add_string b "$N("; B.add_string b (string_of_int tag); B.add_string b ", ");
      for i=0 to size-1 do
        loop (Obj.field o i);
        if i < size - 1 then B.add_string b ", "
      done;
      B.add_char b ')' in
  loop o;
  B.contents b

let invalid s = raise (Invalid_argument ("unserialize: invalid " ^ s))

type token =
    | Int of string
    | Float of string
    | String of string
    | Block_start
    | Block_end
    | EOI

let rec token lb = lexer
  | '[' -> Block_start
  | ']' -> Block_end
  | '-'? ['0'-'9']+ 65535 -> Int (Ulexing.latin1_sub_lexeme lb 0 (Ulexing.lexeme_length lb - 1))
  | '-'? ['0'-'9']+ '.' ['0'-'9']* 65535 -> Float (Ulexing.latin1_sub_lexeme lb 0 (Ulexing.lexeme_length lb - 1))
  | 's' [^ 65535]* 65535 -> String (Ulexing.utf8_sub_lexeme lb 1 (Ulexing.lexeme_length lb - 2))
  | eof -> EOI
  | _ -> invalid ("character " ^ string_of_int (Ulexing.lexeme_char lb 0))

let unserialize s =
  let lb = Ulexing.from_utf8_string s in
  let next_tok () = try token lb lb with Ulexing.Error | Ulexing.InvalidCodepoint _ -> invalid "character" in
  let rec loop () =
    match next_tok () with
      | Int s -> (try Obj.repr (int_of_string s) with Invalid_argument "int_of_string" -> invalid ("int " ^ s))
      | Float s -> (try Obj.repr (float_of_string s) with Invalid_argument "float_of_string" -> invalid ("float " ^ s))
      | String s -> Obj.repr s
      | Block_start ->
          let rec loop2 block =
            match next_tok () with
              | EOI -> invalid "serialized heap object"
              | Block_end ->
                  begin
                    match block with
                      | tag :: fields ->
                          let len = List.length fields in
                          if not (Obj.is_int tag) || len = 0 then invalid "block";
                          let b = Obj.new_block (Obj.obj tag) len in
                          let rec loop3 i = function
                            | [] -> ()
                            | h::t -> Obj.set_field b i h; loop3 (i - 1) t in
                          loop3 (len - 1) fields;
                          b
                      | [] -> invalid "block"
                  end
              | _ -> Ulexing.rollback lb; loop2 (loop () :: block) in
          loop2 []
      | EOI | Block_end -> invalid "serialized heap object" in
  let o = loop () in
  match next_tok () with
    | EOI -> o
    | _ -> invalid "serialized heap object"

let handler procs (cgi : Netcgi_types.cgi_activation) =
  let o = unserialize (cgi#argument "BODY")#value in
  if Obj.is_int o || Obj.tag o <> 0 || Obj.size o <> 2 then raise (Invalid_argument "bad request");
  let (proc_name, arg) = Obj.obj o in
  if Obj.is_int proc_name || Obj.tag proc_name <> Obj.string_tag then raise (Invalid_argument "bad request");
  let proc =
    try List.assoc (Obj.obj proc_name) procs
    with Not_found -> raise (Invalid_argument ("bad request " ^ Obj.obj proc_name)) in
  let res = serialize (proc arg) in

  (* XXX handle gzip *)
  cgi#set_header
    ~content_type:"text/plain; charset=utf-8"
    ~cache:`No_cache
    ();
  cgi#output#output_string res;
  cgi#output#commit_work ()
