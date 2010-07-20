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

(*
  we essentially pass the heap representation back and forth, but the
  heap rep is not identical on client and server: on the Javascript
  side bools are bools, but on the OCaml side they are 0 and 1. for
  this reason we need type-directed marshaling from server to
  client. from client to server we need type-directed unmarshaling
  because we don't trust the client to send us an acceptable heap
  value.

  on the client we walk the heap (without type-direction) to marshal a
  value, and eval a marshaled value to get a heap value. the client ->
  server marshaled format (which does not need to be valid Javascript)
  is not the same as the server -> client one (which does).
*)

type obj =
  | Obool of bool
  | Onumber of float
  | Ostring of string
  | Oblock of int * obj array

let to_unit = function
  | Onumber 0. -> ()
  | _ -> raise (Invalid_argument "unit")

let to_int = function
  | Onumber f -> int_of_float f
  | _ -> raise (Invalid_argument "int")

let to_int32 = function
  | Onumber f -> Int32.of_float f
  | _ -> raise (Invalid_argument "int32")

let to_int64 = function
  | Onumber f -> Int64.of_float f
  | _ -> raise (Invalid_argument "int64")

let to_float = function
  | Onumber f -> f
  | _ -> raise (Invalid_argument "float")

let to_bool = function
  | Obool b -> b
  | _ -> raise (Invalid_argument "bool")

let to_char = function
  | Onumber c -> char_of_int (int_of_float c)
  | _ -> raise (Invalid_argument "char")

let to_string = function
  | Ostring s -> s
  | _ -> raise (Invalid_argument "string")

let to_list to'a x =
  let rec loop = function
    | Onumber 0. -> []
    | Oblock (0, [| x0; x1 |]) -> to'a x0 :: loop x1
    | _ -> raise (Invalid_argument "list") in
  loop x

let to_option to'a = function
  | Onumber 0. -> None
  | Oblock (0, [| x0 |]) -> Some (to'a x0)
  | _ -> raise (Invalid_argument "option")

let to_array to'a = function
  | Oblock (0, a) -> Array.map to'a a
  | _ -> raise (Invalid_argument "array")

let to_ref to'a = function
  | Oblock (0, [| x |]) -> ref (to'a x)
  | _ -> raise (Invalid_argument "ref")

let of_unit () = Onumber 0.
let of_int i = Onumber (float_of_int i)
let of_int32 i = Onumber (Int32.to_float i)
let of_int64 i = Onumber (Int64.to_float i)
let of_float f = Onumber f
let of_bool b = Obool b
let of_char c = Onumber (float_of_int (int_of_char c))
let of_string s = Ostring s

let of_list of'a x =
  let rec loop = function
    | [] -> Onumber 0.
    | h :: t -> Oblock (0, [| of'a h; loop t |]) in
  loop x

let of_option of'a = function
  | None -> Onumber 0.
  | Some x -> Oblock (0, [| of'a x |])

let of_array of'a a = Oblock (0, Array.map of'a a)

let of_ref of'a x = Oblock (0, [| of'a !x |])

let serialize o =
  let module B = Buffer in
  let b = B.create 1024 in
  let rec loop = function
    | Obool bl -> B.add_string b (if bl then "true" else "false")
    | Onumber f -> B.add_string b (string_of_float f)
    | Ostring s -> B.add_char b '"'; B.add_string b (Jslib_pp.escaped s); B.add_char b '"'
    | Oblock (tag, fields) ->
        begin match tag with
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
        let size = Array.length fields in
        for i=0 to size-1 do
          loop fields.(i);
          if i < size - 1 then B.add_string b ", "
        done;
        if tag > 9 then B.add_char b ']';
        B.add_char b ')' in
  loop o;
  B.contents b

let invalid s = raise (Invalid_argument ("unserialize: invalid " ^ s))

type token =
  | Tbool of bool
  | Tnumber of float
  | Tstring of string
  | Tblock_start
  | Tblock_end
  | Teoi

let rec token lb = lexer
  | '[' -> Tblock_start
  | ']' -> Tblock_end
  | '-'? ['0'-'9']+ ( '.' ['0'-'9']* )? 65535 ->
      let s = Ulexing.latin1_sub_lexeme lb 0 (Ulexing.lexeme_length lb - 1) in
      (try Tnumber (float_of_string s) with Invalid_argument "float_of_string" -> invalid s)
  | 's' [^ 65535]* 65535 -> Tstring (Ulexing.utf8_sub_lexeme lb 1 (Ulexing.lexeme_length lb - 2))
  | 't' -> Tbool true
  | 'f' -> Tbool false
  | eof -> Teoi
  | _ -> invalid ("character " ^ string_of_int (Ulexing.lexeme_char lb 0))

let unserialize s =
  !debug ("unserialize " ^ s);
  let lb = Ulexing.from_utf8_string s in
  let next_tok () = try token lb lb with Ulexing.Error | Ulexing.InvalidCodepoint _ -> invalid "character" in
  let rec loop () =
    match next_tok () with
      | Tbool b -> Obool b
      | Tnumber f -> Onumber f
      | Tstring s -> Ostring s
      | Tblock_start ->
          let rec loop2 block =
            match next_tok () with
              | Teoi -> invalid "serialized heap object"
              | Tblock_end ->
                  begin
                    match block with
                      | Onumber tag :: ((_::_) as fields) -> Oblock (int_of_float tag, Array.of_list (List.rev fields))
                      | _ -> invalid "block"
                  end
              | _ -> Ulexing.rollback lb; loop2 (loop () :: block) in
          loop2 []
      | Teoi | Tblock_end -> invalid "serialized heap object" in
  let o = loop () in
  match next_tok () with
    | Teoi -> o
    | _ -> invalid "serialized heap object"

module type Monad =
sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Sync =
struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
end

module Handler (M : Monad) =
struct
  let handler procs body =
    let (proc_name, arg) =
      match unserialize body with
        | Oblock (0, [| Ostring proc_name; arg |]) -> proc_name, arg
        | _ -> raise (Invalid_argument "bad request") in
    let proc =
      try List.assoc proc_name procs
      with Not_found -> raise (Invalid_argument ("bad request " ^ proc_name)) in
    M.bind (proc arg) (fun s -> M.return (serialize s))
end
