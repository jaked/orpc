val x_char : Xdr.xdr_type_term
val x_list : Xdr.xdr_type_term -> Xdr.xdr_type_term
val to_list : (Xdr.xdr_value -> 'a) -> Xdr.xdr_value -> 'a list
val of_list : ('a -> Xdr.xdr_value) -> 'a list -> Xdr.xdr_value
val to_option : (Xdr.xdr_value -> 'a) -> Xdr.xdr_value -> 'a option
val of_option : ('a -> Xdr.xdr_value) -> 'a option -> Xdr.xdr_value
