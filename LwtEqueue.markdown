---
layout: page
title: Lwt on top of Equeue
---
#Lwt on top of Equeue

To support the Lwt kind of [modules interface] (ModulesInterfaces.html),
`orpc` includes a port of the [Lwt] (http://ocsigen.org/lwt)
cooperative threads library to run on top of Ocamlnet's Equeue,
`lwt-equeue`. (XXX this is currently based on LWT 1.x; the next version
of orpc will support LWT 2.x.)

Most of Lwt is unchaged; the `Lwt_unix` module is reimplemented in
terms of Equeue operations. There is a new `Lwt_equeue` module for
setting the event system. The intention is that `lwt-equeue` be
link-compatible with regular `Lwt`.

In addition there are some new modules for condition variables,
blocking queues and blocking priority queues.

See the [Ocamldoc] (doc/index.html).
