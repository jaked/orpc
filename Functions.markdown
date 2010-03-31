---
layout: page
title: Functions
---
#Functions and function arguments

| orpc declaration                    | ONC RPC equivalent         |
|-------------------------------------|----------------------------|
| `val ping : unit -> unit`           | `void ping(void) = 0`      |
| `val add : int -> int -> int`       | `int add(int,int,int) = 0` |
| `val add : (int * int) -> int`      | `int add(int,int,int) = 0` |
| `val add : ?m:int -> int -> int`    | `int add(int *, int) = 0`  |
| `val add : ~m:int -> ~n:int -> int` | `int add(int, int) = 0`    |

Remote procedures are declared as OCaml functions. It's not necessary
(or possible) to give an RPC function number; functions are numbered
from 0 in the order they appear in the file.

Multiple arguments may be given curried or tupled; either way you get
the same ONC RPC equivalent. Labelled and optional arguments may also
be used, although of course the labels do not show up in the ONC RPC
equivalent.

Any built-in types or types that have been previously been declared in
the input file may be given as function argument types, subject to the
following restrictions (in all cases because there is no sensible way
to marshal values):

 * Higher-order functions are not supported
 * `exn` is not supported (but see [Exceptions] (Exceptions.html))
 * Polymorphic types must be instantiated; in particular polymorphic
   variants must be closed

It is also possible to use types declared in another module; see
[Type-conv support] (Typeconv.html).
