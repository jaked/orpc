---
layout: page
title: Tracing modules
---
#Tracing modules

In addition to the standard `Protocol_{aux|clnt|srv}` modules, orpc
generates `Protocol_trace` with the following signature (see
[ModulesInterfaces] (ModulesInterfaces.html) for the input file):

{% highlight ocaml %}
module type Pp =
  sig
    val pp_foo : Format.formatter -> Protocol.foo -> unit;;
    val pp_baz'call : Format.formatter -> Protocol.foo -> unit;;
    val pp_baz'reply : Format.formatter -> int -> unit;;
    val pp_exn : Format.formatter -> exn -> unit;;
    val pp_exn'reply : Format.formatter -> exn -> unit;;
  end;;
module Pp_pp (P : Pp) : Pp;;
module Pp : Pp;;
module Sync_pp (P : Pp) (T : Orpc.Trace) (A : Protocol.Sync) : Protocol.
  Sync;;
module Sync (T : Orpc.Trace) (A : Protocol.Sync) : Protocol.Sync;;
module Async_pp (P : Pp) (T : Orpc.Trace) (A : Protocol.Async) : Protocol.
  Async;;
module Async (T : Orpc.Trace) (A : Protocol.Async) : Protocol.Async;;
module Lwt_pp (P : Pp) (T : Orpc.Trace) (A : Protocol.Lwt) : Protocol.Lwt;;
module Lwt (T : Orpc.Trace) (A : Protocol.Lwt) : Protocol.Lwt;;
{% endhighlight %}

The `pp_*` functions print data types defined in the input to a
`Format.formatter`, and the functors wrap a client or server to trace
calls across the interface.

The idea with the `Pp` signature is as follows:

 * `module type Pp` is a signature for pretty-printing types defined in the input and calls across the interface.
 * `module Pp` is an implementation that defines default pretty-printing behavior.
 * `Pp_pp` lets you override the default behavior. E.g., `module Pp` is defined as `module rec Pp : Pp = struct include Pp_pp(Pp) end`; for custom behavior, start with this then add overrides after the `include`.
 * `Sync` et al. use the default pretty-printing behavior; `Sync_pp` et al. let you override it.
