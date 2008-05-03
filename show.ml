(* deriving/lib/show.ml, run through deriving *)
module Show =
  struct
    (** Show **)
    module type Show =
      sig
        type a
        val format : Format.formatter -> a -> unit
        val format_list : Format.formatter -> a list -> unit
        val show : a -> string
        val show_list : a list -> string
      end
    module type SimpleFormatter =
      sig type a val format : Format.formatter -> a -> unit end
    module ShowFormatterDefault (S : SimpleFormatter) =
      struct
        include S
        let format_list formatter items =
          let rec writeItems formatter =
            function
            | [] -> ()
            | [ x ] -> S.format formatter x
            | x :: xs ->
                Format.fprintf formatter "%a;@;%a" S.format x writeItems xs
          in Format.fprintf formatter "@[<hov 1>[%a]@]" writeItems items
      end
    module ShowDefaults'
      (S :
        sig
          type a
          val format : Format.formatter -> a -> unit
          val format_list : Format.formatter -> a list -> unit
        end) :
      Show with type a = S.a =
      struct
        include S
        let showFormatted f item =
          let b = Buffer.create 16 in
          let formatter = Format.formatter_of_buffer b
          in
            (Format.fprintf formatter "@[<hov 0>%a@]@?" f item;
             Buffer.sub b 0 (Buffer.length b))
        (* Warning: do not eta-reduce either of the following *)
        let show item = showFormatted S.format item
        let show_list items = showFormatted S.format_list items
      end
    module Defaults (S : SimpleFormatter) : Show with type a = S.a =
      ShowDefaults'(ShowFormatterDefault(S))
    module Show_unprintable (S : sig type a end) =
      (*: Show with type a = S.a *)
      Defaults
        (struct
           type a = S.a
           let format formatter _ = Format.pp_print_string formatter "..."
         end)
    (* instance Show a => Show [a] *)
    module Show_list (S : Show) : Show with type a = S.a list =
      Defaults(struct type a = S.a list let format = S.format_list end)
    (* instance Show a => Show (a option) *)
    module Show_option (S : Show) : Show with type a = S.a option =
      Defaults
        (struct
           type a = S.a option
           let format formatter =
             function
             | None -> Format.fprintf formatter "@[None@]"
             | Some s ->
                 Format.fprintf formatter "@[Some@;<1 2>%a@]" S.format s
         end)
    (* instance Show a => Show (a lazy_t) *)
    module Show_lazy_t (S : Show) : Show with type a = S.a lazy_t =
      Defaults
        (struct
           type a = S.a lazy_t
           let format formatter s = S.format formatter (Lazy.force s)
         end)
    (* instance Show a => Show (a array) *)
    module Show_array (S : Show) : Show with type a = S.a array =
      Defaults
        (struct
           type a = S.a array
           let format formatter obj =
             let writeItems formatter items =
               let length = Array.length items
               in
                 (for i = 0 to length - 2 do
                    Format.fprintf formatter "@[%a;@;@]" S.format
                      (Array.get items i)
                  done;
                  if length <> 0
                  then S.format formatter (Array.get items (length - 1))
                  else ())
             in Format.fprintf formatter "@[[|%a|]@]" writeItems obj
         end)
    module Show_map
      (O : Map.OrderedType) (K : Show with type a = O.t) (V : Show) :
      Show with type a = V.a Map.Make(O).t =
      Defaults
        (struct
           module M = Map.Make(O)
           type a = V.a M.t
           let format formatter map =
             (Format.pp_open_box formatter 0;
              Format.pp_print_string formatter "{";
              M.iter
                (fun key value ->
                   (Format.pp_open_box formatter 0;
                    K.format formatter key;
                    Format.pp_print_string formatter " => ";
                    V.format formatter value;
                    Format.pp_close_box formatter ()))
                map;
              Format.pp_print_string formatter "}";
              Format.pp_close_box formatter ())
         end)
    module Show_set (O : Set.OrderedType) (K : Show with type a = O.t) :
      Show with type a = Set.Make(O).t =
      Defaults
        (struct
           module S = Set.Make(O)
           type a = S.t
           let format formatter set =
             (Format.pp_open_box formatter 0;
              Format.pp_print_string formatter "{";
              S.iter
                (fun elt ->
                   (Format.pp_open_box formatter 0;
                    K.format formatter elt;
                    Format.pp_close_box formatter ()))
                set;
              Format.pp_print_string formatter "}";
              Format.pp_close_box formatter ())
         end)
    module Show_bool =
      Defaults
        (struct
           type a = bool
           let format formatter item =
             match item with
             | true -> Format.pp_print_string formatter "true"
             | false -> Format.pp_print_string formatter "false"
         end)
    module Show_integer (S : sig type t val to_string : t -> string end) =
      Defaults
        (struct
           type a = S.t
           let format formatter item =
             Format.pp_print_string formatter (S.to_string item)
         end)
    module Show_int32 = Show_integer(Int32)
    module Show_int64 = Show_integer(Int64)
    module Show_nativeint = Show_integer(Nativeint)
    module Show_char =
      Defaults
        (struct
           type a = char
           let format formatter item =
             Format.pp_print_string formatter
               ("'" ^ ((Char.escaped item) ^ "'"))
         end)
    module Show_int =
      Defaults
        (struct
           type a = int
           let format formatter item =
             Format.pp_print_string formatter (string_of_int item)
         end)
    module Show_float =
      Defaults
        (struct
           type a = float
           let format formatter item =
             Format.pp_print_string formatter (string_of_float item)
         end)
    module Show_string =
      Defaults
        (struct
           type a = string
           let format formatter item =
             (Format.pp_print_char formatter '"';
              Format.pp_print_string formatter (String.escaped item);
              Format.pp_print_char formatter '"')
         end)
    module Show_unit =
      Defaults
        (struct
           type a = unit
           let format formatter () = Format.pp_print_string formatter "()"
         end)
  end
include Show
type open_flag =
  Pervasives.open_flag =
    | Open_rdonly | Open_wronly | Open_append | Open_creat | Open_trunc
    | Open_excl | Open_binary | Open_text | Open_nonblock
open Show
module rec Show_open_flag : Show.Show with type a = open_flag =
             Show.Defaults
               (struct
                  type a = open_flag
                  let format formatter =
                    function
                    | Open_rdonly ->
                        Format.pp_print_string formatter "Open_rdonly"
                    | Open_wronly ->
                        Format.pp_print_string formatter "Open_wronly"
                    | Open_append ->
                        Format.pp_print_string formatter "Open_append"
                    | Open_creat ->
                        Format.pp_print_string formatter "Open_creat"
                    | Open_trunc ->
                        Format.pp_print_string formatter "Open_trunc"
                    | Open_excl ->
                        Format.pp_print_string formatter "Open_excl"
                    | Open_binary ->
                        Format.pp_print_string formatter "Open_binary"
                    | Open_text ->
                        Format.pp_print_string formatter "Open_text"
                    | Open_nonblock ->
                        Format.pp_print_string formatter "Open_nonblock"
                end)
type fpclass =
  Pervasives.fpclass =
    | FP_normal | FP_subnormal | FP_zero | FP_infinite | FP_nan
open Show
module rec Show_fpclass : Show.Show with type a = fpclass =
             Show.Defaults
               (struct
                  type a = fpclass
                  let format formatter =
                    function
                    | FP_normal ->
                        Format.pp_print_string formatter "FP_normal"
                    | FP_subnormal ->
                        Format.pp_print_string formatter "FP_subnormal"
                    | FP_zero -> Format.pp_print_string formatter "FP_zero"
                    | FP_infinite ->
                        Format.pp_print_string formatter "FP_infinite"
                    | FP_nan -> Format.pp_print_string formatter "FP_nan"
                end)
type 'a ref = ('a Pervasives.ref) = { mutable contents : 'a }
module Show_BVSP8sG8boYWAXE1beAXFPo2KTW3UFQm (V_a : Show.Show) =
  struct
    open Show
    module rec Show_ref : Show.Show with type a = V_a.a ref =
                 Show.Defaults
                   (struct
                      type a = V_a.a ref
                      let format formatter { contents = contents } =
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_char formatter '{';
                         Format.pp_print_string formatter "contents =";
                         V_a.format formatter contents;
                         Format.pp_print_char formatter '}';
                         Format.pp_close_box formatter ())
                    end)
  end
module Show_ref (V_a : Show.Show) =
  struct
    module P = Show_BVSP8sG8boYWAXE1beAXFPo2KTW3UFQm(V_a)
    include P.Show_ref
  end
