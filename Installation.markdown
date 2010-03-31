---
layout: page
title: Installing and running orpc
---
#Installing and running orpc

##Installing orpc

 1. Prerequisites: OCaml 3.10.2 or 3.11.x, [Findlib] (http://projects.camlcity.org/projects/findlib.html ),
and [Ocamlnet] (http://projects.camlcity.org/projects/ocamlnet.html)
 2. `./configure [-disable-ocamljs] [-disable-ssl]` configures orpc (optionally disabling ocamljs or SSL support)
 3. `make` builds orpc
 4. `make install` installs orpc and the associated libraries
 5. `make examples` builds all the examples

##Using orpc

Run orpc with a [simple-] (SimpleInterfaces.html) or [modules-style] (ModuleInterfaces.html)
input file as argument. There is one command-line option, `--js`,
which controls whether client and server are generated for ONC RPC or
for use with ocamljs for [RPC over HTTP] (RpcOverHttp.html).

There are several examples in the `examples` directory showing various
features of orpc and how to build programs using it with ocamlbuild.
