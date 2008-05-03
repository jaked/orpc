(* simplified AST for the things we handle, run through deriving with Show *)

(* dump locations in ASTs *)

module Loc =
struct
  type t = Camlp4.PreCast.Loc.t
  open Show
  module rec Show_t : Show.Show with type a = t =
    Show.Defaults
      (struct
        type a = t
        let format formatter t = Camlp4.PreCast.Loc.dump formatter t
      end)
end

type ident = string

open Show
  
module rec Show_ident : Show.Show with type a = string =
             Show.Defaults(Show_string)
  
type typ =
  | Var of Loc.t * ident
  | Unit of Loc.t
  | Int of Loc.t
  | Int32 of Loc.t
  | Int64 of Loc.t
  | Float of Loc.t
  | Bool of Loc.t
  | Char of Loc.t
  | String of Loc.t
  | Tuple of Loc.t * typ list
  | Record of Loc.t * (ident * typ) list
  | Variant of Loc.t * (ident * (typ list)) list
  | Array of Loc.t * typ
  | List of Loc.t * typ
  | Option of Loc.t * typ
  | Apply of Loc.t * ident * typ list

open Show
  
module rec Show_typ : Show.Show with type a = typ =
             Show.Defaults
               (struct
                  type a = typ
                  
                  let format formatter =
                    function
                    | Var ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Var";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0 Show_ident.format v1;
                         Format.pp_close_box formatter ())
                    | Unit v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Unit";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Int v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Int";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Int32 v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Int32";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Int64 v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Int64";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Float v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Float";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Bool v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Bool";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Char v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Char";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | String v0 ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "String";
                         Format.pp_print_break formatter 1 2;
                         Loc.Show_t.format formatter v0;
                         Format.pp_close_box formatter ())
                    | Tuple ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Tuple";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0
                           (let module M = Show_list(Show_typ) in M.format)
                           v1;
                         Format.pp_close_box formatter ())
                    | Record ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Record";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0
                           (let module M =
                              Show_list
                                (Defaults
                                   (struct
                                      type a = (ident * typ)
                                      
                                      let format formatter (v0, v1) =
                                        Format.fprintf formatter
                                          "@[<hov 1>(%a,@;%a)@]" Show_ident.
                                          format v0 Show_typ.format v1
                                        
                                    end))
                           in M.format) v1;
                         Format.pp_close_box formatter ())
                    | Variant ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Variant";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0
                           (let module M =
                              Show_list
                                (Defaults
                                   (struct
                                      type a = (ident * (typ list))
                                      
                                      let format formatter (v0, v1) =
                                        Format.fprintf formatter
                                          "@[<hov 1>(%a,@;%a)@]" Show_ident.
                                          format v0
                                          (let module M = Show_list(Show_typ)
                                          in M.format) v1
                                        
                                    end))
                           in M.format) v1;
                         Format.pp_close_box formatter ())
                    | Array ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Array";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0 Show_typ.format v1;
                         Format.pp_close_box formatter ())
                    | List ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "List";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0 Show_typ.format v1;
                         Format.pp_close_box formatter ())
                    | Option ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Option";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]" Loc.
                           Show_t.format v0 Show_typ.format v1;
                         Format.pp_close_box formatter ())
                    | Apply ((v0, v1, v2)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Apply";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a)@]"
                           Loc.Show_t.format v0 Show_ident.format v1
                           (let module M = Show_list(Show_typ) in M.format)
                           v2;
                         Format.pp_close_box formatter ())
                    
                end)
  
type typedef = (Loc.t * (ident list) * ident * typ) list

open Show
  
module rec Show_typedef :
             Show.Show with
               type a = (Loc.t * (ident list) * ident * typ) list =
             Show.Defaults
               (Show_list
                  (Defaults
                     (struct
                        type a = (Loc.t * (ident list) * ident * typ)
                        
                        let format formatter (v0, v1, v2, v3) =
                          Format.fprintf formatter
                            "@[<hov 1>(%a,@;%a,@;%a,@;%a)@]" Loc.Show_t.
                            format v0 (let module M = Show_list(Show_ident)
                            in M.format) v1 Show_ident.format v2 Show_typ.
                            format v3
                          
                      end)))
  
type func = (Loc.t * ident * (typ list) * typ)

open Show
  
module rec Show_func :
             Show.Show with type a = (Loc.t * ident * (typ list) * typ) =
             Show.Defaults
               (Defaults
                  (struct
                     type a = (Loc.t * ident * (typ list) * typ)
                     
                     let format formatter (v0, v1, v2, v3) =
                       Format.fprintf formatter
                         "@[<hov 1>(%a,@;%a,@;%a,@;%a)@]" Loc.Show_t.format
                         v0 Show_ident.format v1
                         (let module M = Show_list(Show_typ) in M.format) v2
                         Show_typ.format v3
                       
                   end))
  
type module_type = (Loc.t * ident * (func list))

open Show
  
module rec Show_module_type :
             Show.Show with type a = (Loc.t * ident * (func list)) =
             Show.Defaults
               (Defaults
                  (struct
                     type a = (Loc.t * ident * (func list))
                     
                     let format formatter (v0, v1, v2) =
                       Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a)@]"
                         Loc.Show_t.format v0 Show_ident.format v1
                         (let module M = Show_list(Show_func) in M.format) v2
                       
                   end))
  
type interface =
  | Simple of typedef list * func list
  | Modules of typedef list * module_type * module_type option

open Show
  
module rec Show_interface : Show.Show with type a = interface =
             Show.Defaults
               (struct
                  type a = interface
                  
                  let format formatter =
                    function
                    | Simple ((v0, v1)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Simple";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]"
                           (let module M = Show_list(Show_typedef)
                           in M.format) v0
                           (let module M = Show_list(Show_func) in M.format)
                           v1;
                         Format.pp_close_box formatter ())
                    | Modules ((v0, v1, v2)) ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "Modules";
                         Format.pp_print_break formatter 1 2;
                         Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a)@]"
                           (let module M = Show_list(Show_typedef)
                           in M.format) v0 Show_module_type.format v1
                           (let module M = Show_option(Show_module_type)
                           in M.format) v2;
                         Format.pp_close_box formatter ())
                    
                end)
  

