#summary Installing and running orpc
#labels Featured

===Installing orpc===

  # Prerequisites: OCaml 3.10.2 or 3.11.0, [http://projects.camlcity.org/projects/findlib.html Findlib], and [http://projects.camlcity.org/projects/ocamlnet.html Ocamlnet]
  # `./configure [-disable-ocamljs] [-disable-ssl]` configures orpc (optionally disabling ocamljs or SSL support)
  # `make` builds orpc
  # `make install` installs orpc and the associated libraries
  # `make examples` builds all the examples

===Using orpc===
Run orpc with a simple- or modules-style input file as argument (see
[SimpleInterfaces] or [ModulesInterfaces]). There is one command-line
option, `--js`, which controls whether client and server are generated
for ONC RPC or for use with ocamljs for [RpcOverHttp RPC over HTTP].

There are several examples in the `examples` directory showing various
features of orpc and how to build programs using it with ocamlbuild.
