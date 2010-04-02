Orpc is a tool for generating RPC clients and servers. It can generate
clients / servers for use with Ocamlnet's ONC RPC implementation (like
ocamlrpcgen), and also for RPC over HTTP for use with
[ocamljs] (http://github.com/jaked/ocamljs). To describe interfaces,
you give orpc an OCaml signature instead of an RFC-1831/1832-format
file. Most OCaml types (including polymorphic types), exceptions, and
functions with labelled/optional arguments are supported.

See [http://jaked.github.com/orpc] (http://jaked.github.com/orpc) for details.

For a quick start:

 1. ./configure (-disable-ocamljs if you do not have ocamljs)
 2. make
 3. make install
 4. make examples

Orpc is written by Jake Donham, with contributions from Mike Wells.
