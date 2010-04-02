(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 * Copyright (C) 2010 Jacob Donham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

let debug = ref ignore
let set_debug f = debug := f

type obj =
    | Oint of int
    | Ofloat of float
    | Ostring of string
    | Oblock of int * obj array

let to_unit = function
  | Oint 0 -> ()
  | _ -> raise (Invalid_argument "unit")

let to_int = function
  | Oint i -> i
  | _ -> raise (Invalid_argument "int")

let to_int32 = function
  | Oint i -> Int32.of_int i
  | Ofloat f -> Int32.of_float f
  | _ -> raise (Invalid_argument "int32")

let to_int64 = function
  | Oint i -> Int64.of_int i
  | Ofloat f -> Int64.of_float f
  | _ -> raise (Invalid_argument "int64")

let to_float = function
  | Oint i -> float_of_int i
  | Ofloat f -> f
  | _ -> raise (Invalid_argument "float")

let to_bool = function
  | Oint 0 -> false
  | Oint 1 -> true
  | _ -> raise (Invalid_argument "bool")

let to_char = function
  | Oint c -> char_of_int c
  | _ -> raise (Invalid_argument "char")

let to_string = function
  | Ostring s -> s
  | _ -> raise (Invalid_argument "string")

let to_list to'a x =
  let rec loop = function
    | Oint 0 -> []
    | Oblock (0, [| x0; x1 |]) -> to'a x0 :: loop x1
    | _ -> raise (Invalid_argument "list") in
  loop x

let to_option to'a = function
  | Oint 0 -> None
  | Oblock (0, [| x0 |]) -> Some (to'a x0)
  | _ -> raise (Invalid_argument "option")

(*
  in essence we are transferring the heap representation back and
  forth, but since it is not identical on client and server we do
  things asymmetrically:

  from server to client we can work directly over the heap
  representation; we have enough information to generate the right
  Javascript heap object.

  from client to server we don't have complete information (e.g. the
  client doesn't know if a particular Javascript number is supposed to
  be a float or an int). the wire format doesn't distinguish between
  float and int; we parse it to an intermediate "obj" (for numbers we
  guess that if there is a period it's a float); then we use the types
  in the orpc interface to guide the ultimate conversion (see the to_*
  functions in *_aux.ml).
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
      begin
        match tag with
          | 0 -> B.add_string b "$("
          | 1 -> B.add_string b "$1("
          | 2 -> B.add_string b "$2("
          | 3 -> B.add_string b "$3("
          | 4 -> B.add_string b "$4("
          | 5 -> B.add_string b "$5("
          | 6 -> B.add_string b "$6("
          | 7 -> B.add_string b "$7("
          | 8 -> B.add_string b "$8("
          | 9 -> B.add_string b "$9("
          | _ -> B.add_string b "$N("; B.add_string b (string_of_int tag); B.add_string b ", ["
      end;
      for i=0 to size-1 do
        loop (Obj.field o i);
        if i < size - 1 then B.add_string b ", "
      done;
      if tag > 9 then B.add_char b ']';
      B.add_char b ')' in
  loop o;
  B.contents b

let invalid s = raise (Invalid_argument ("unserialize: invalid " ^ s))

type token =
    | Tint of string
    | Tfloat of string
    | Tstring of string
    | Tblock_start
    | Tblock_end
    | Teoi

let rec token lb = lexer
  | '[' -> Tblock_start
  | ']' -> Tblock_end
  | '-'? ['0'-'9']+ 65535 -> Tint (Ulexing.latin1_sub_lexeme lb 0 (Ulexing.lexeme_length lb - 1))
  | '-'? ['0'-'9']+ '.' ['0'-'9']* 65535 -> Tfloat (Ulexing.latin1_sub_lexeme lb 0 (Ulexing.lexeme_length lb - 1))
  | 's' [^ 65535]* 65535 -> Tstring (Ulexing.utf8_sub_lexeme lb 1 (Ulexing.lexeme_length lb - 2))
  | eof -> Teoi
  | _ -> invalid ("character " ^ string_of_int (Ulexing.lexeme_char lb 0))

let unserialize s =
  !debug ("unserialize " ^ s);
  let lb = Ulexing.from_utf8_string s in
  let next_tok () = try token lb lb with Ulexing.Error | Ulexing.InvalidCodepoint _ -> invalid "character" in
  let rec loop () =
    match next_tok () with
      | Tint s ->
          begin
            try Oint (int_of_string s)
            with Invalid_argument "int_of_string" ->
              try Ofloat (float_of_string s) (* in case it's out of range *)
              with Invalid_argument "float_of_string" ->
                invalid ("int " ^ s)
          end
      | Tfloat s ->
          begin
            try Ofloat (float_of_string s)
            with Invalid_argument "float_of_string" -> invalid ("float " ^ s)
          end
      | Tstring s -> Ostring s
      | Tblock_start ->
          let rec loop2 block =
            match next_tok () with
              | Teoi -> invalid "serialized heap object"
              | Tblock_end ->
                  begin
                    match block with
                      | Oint tag :: ((_::_) as fields) -> Oblock (tag, Array.of_list (List.rev fields))
                      | _ -> invalid "block"
                  end
              | _ -> Ulexing.rollback lb; loop2 (loop () :: block) in
          loop2 []
      | Teoi | Tblock_end -> invalid "serialized heap object" in
  let o = loop () in
  match next_tok () with
    | Teoi -> o
    | _ -> invalid "serialized heap object"

let handler procs body =
  let (proc_name, arg) =
    match unserialize body with
      | Oblock (0, [| Ostring proc_name; arg |]) -> proc_name, arg
      | _ -> raise (Invalid_argument "bad request") in
  let proc =
    try List.assoc proc_name procs
    with Not_found -> raise (Invalid_argument ("bad request " ^ proc_name)) in
  serialize (proc arg)

let service handler =
  let process (cgi : Netcgi_types.cgi_activation) =
    let res =
      try handler (cgi#argument "BODY")#value
      with Not_found -> raise (Invalid_argument "bad_request") in
    (* XXX handle gzip *)
    cgi#set_header
      ~content_type:"text/plain; charset=utf-8"
      ~cache:`No_cache
      ();
    cgi#output#output_string res;
    cgi#output#commit_work () in

  {
    Nethttpd_services.dyn_handler = (fun _ -> process);
    dyn_activation = Nethttpd_services.std_activation `Std_activation_unbuffered;
    dyn_uri = None;
    dyn_translator = (fun _ -> "");
    dyn_accept_all_conditionals = false;
  }
