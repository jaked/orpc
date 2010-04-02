---
layout: page
title: Externally defined types
---
#Externally defined types

It is possible to have marshalling functions (and tracing functions)
generated for types defined outside an `orpc` interface file. This is
useful if you want to use the same types in more than one RPC
protocol, if you have some existing types which you would like to
transmit over RPC without having to move them into the interface file,
and if you would like to keep types abstract but still be able to
marshal them (however `orpc` makes no attempt to enforce abstraction
across the RPC interface).

To use an external type in an interface, just refer to it by its
module-qualified name. To generate the marshalling functions, build
with Camlp4 and the `orpc-syntax` package, and define your types like

{% highlight ocaml %}
type foo = Bar | Baz of int with orpc(aux, trace)
{% endhighlight %}

Generators are given as a comma-separated list after `with orpc`; the
available generators are as follows:

| tag      | generator                     |
|----------|-------------------------------|
| `aux`    | marshalling for ONC RPC       |
| `js_aux` | marshalling for RPC over HTTP |
| `trace`  | tracing functions             |

See the `external` example for more details.

You could also use external types to override the marshalling that
`orpc` uses for a particular type. If you want to do this try dumping
out the generated code for a type to see how it works; you can just
replace the functions instead of calling the generator.

If you want to use the same external type (defined in a single module)
over both ONC RPC and RPC over HTTP, it is slightly tricky. The code
for ONC RPC can't be generated when compiling with `ocamljs`, and the
code for RPC over HTTP can't be generated when compiling with
`ocamlc`/`ocamlopt`, because the libraries each depends on are not
available. But there is only one `.cmi` file which is shared between
the two compilations, so you can't just omit the unneeded
functions. To get around this there is a special argument to Camlp4,
`--ocamljs`, which you add when ccompiling with `ocamljs`, so that the
unneeded functions are stubbed out.
