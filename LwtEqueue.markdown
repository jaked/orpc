#summary Lwt on top of Equeue

To support the Lwt kind of modules interface (see
[ModulesInterfaces]), `orpc` includes a port of the
[http://ocsigen.org/lwt Lwt] cooperative threads library to run on top
of Ocamlnet's Equeue, `lwt-equeue`

Most of Lwt is unchaged; the `Lwt_unix` module is reimplemented in
terms of Equeue operations. There is a new `Lwt_equeue` module for
setting the event system. The intention is that `lwt-equeue` be
link-compatible with regular `Lwt`.

In addition there are some new modules for condition variables,
blocking queues and blocking priority queues.

See the [http://orpc2.googlecode.com/svn/doc/index.html Ocamldoc].
