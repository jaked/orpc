#summary Functions and function arguments

Remote procedures are declared as OCaml functions (ONC RPC equivalent
in the second column):

||`val ping : unit -> unit`||`void ping(void) = 0`||

It's not necessary (or possible) to give a function number; functions
are numbered from 0 in the order they appear in the file.

Multiple arguments may be given curried or tupled, with the same ONC
RPC equivalent:

||`val add : int -> int -> int` ||`int add(int,int,int) = 0`||
||`val add : (int * int) -> int`||`int add(int,int,int) = 0`||

Labelled and optional arguments may also be used, although of course
the labels do not show up in the ONC RPC equivalent:

||`val add : ?m:int -> int -> int`   ||`int add(int *, int) = 0`||
||`val add : ~m:int -> ~n:int -> int`||`int add(int, int) = 0`  ||

Any built-in types or types that have been previously been declared in
the input file may be given as function argument types, subject to the
following restrictions:

  * Higher-order functions are not supported
  * Exceptions may not be given as arguments (but see [Exceptions])
  * Polymorphic types must be instantiated
