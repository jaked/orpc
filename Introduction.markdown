---
layout: page
title: Introduction to orpc
---
#Introduction to orpc

Orpc is a tool for generating RPC clients and servers. It can generate
clients / servers for use with Ocamlnet's ONC RPC implementation (like
ocamlrpcgen), and also for RPC over HTTP for use with
[ocamljs] (http://github.com/jaked/ocamljs). To describe interfaces,
you give orpc an OCaml signature (instead of an RFC-1831/1832-format
file like ocamlrpcgen). Orpc supports OCaml base types, tuples,
variants, records, and polymorphic variants (but not objects). It
also supports exceptions and named and optional function arguments.

See more about

 * [supported types] (Types.html)
 * [functions and function arguments] (Functions.html)
 * [exceptions] (Exceptions.html)

Clients and servers can be generated in the style of ocamlrpcgen
(collections of functions in synchronous and asynchronous modes) or as
modules in several styles: synchronous, asynchronous with callbacks,
and asynchronous using the [Lwt] (http://ocsigen.org/lwt) cooperative
threads library. The client and server modules have compatible
signatures so you can easily change between direct calls and network
calls; you can also interpose tracing modules (which orpc generates).

See more about

 * [simple (ocamlrpcgen-style) interfaces] (SimpleInterfaces.html)
 * [modules-style interfaces] (ModuleInterfaces.html)
 * [tracing] (Tracing.html)
 * [RPC over HTTP] (RpcOverHttp.html)

Also included is lwt-equeue, a port of Lwt to run on top of Ocamlnet's
equeue. (XXX this is based on Lwt 1.x; the next version of orpc will support Lwt 2.x)

See more about

 * [lwt-equeue] (LwtEqueue.html)

To install orpc, see [Installation] (Installation.html).

See the [Ocamldoc] (doc/index.html).
