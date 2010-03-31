#summary Introduction to orpc
#labels Featured

Orpc is a tool for generating RPC clients and servers. It can generate
clients / servers for use with Ocamlnet's ONC RPC implementation (like
ocamlrpcgen), and also for RPC over HTTP for use with
[http://code.google.com/p/ocamljs/ ocamljs]. To describe interfaces,
you give orpc an OCaml signature (instead of an RFC-1831/1832-format
file like ocamlrpcgen). Orpc supports OCaml base types, tuples,
variants, and records (but not objects or polymorphic variants). It
also supports exceptions and named and optional function arguments.

See more about
  * [Types supported types]
  * [Functions functions and function arguments]
  * [Exceptions exceptions]

Clients and servers can be generated in the style of ocamlrpcgen
(collections of functions in synchronous and asynchronous modes) or as
modules in several styles: synchronous, asynchronous with callbacks,
and asynchronous using the [http://ocsigen.org/lwt Lwt] cooperative
threads library. The client and server modules have compatible
signatures so you can easily change between direct calls and network
calls; you can also interpose tracing modules (which orpc generates).

See more about
  * [SimpleInterfaces simple (ocamlrpcgen-style) interfaces]
  * [ModulesInterfaces modules-style interfaces]
  * [Tracing tracing]
  * [RpcOverHttp RPC over HTTP]

Also included is lwt-equeue, a port of Lwt to run on top of Ocamlnet's
equeue.

See more about
  * [LwtEqueue lwt-equeue]

To install orpc, see [Installation].

See the [http://orpc2.googlecode.com/svn/doc/index.html Ocamldoc].
