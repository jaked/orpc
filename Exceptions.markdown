---
layout: page
title: Exceptions
---
#Exceptions

Exceptions are supported but must be declared in the input file (so
that they can be marshalled). Unfortunately it seems not to be
possible to equate one exception with another and also define the data
it contains, so built-in exceptions cannot be used. (XXX: this is not
true, it is possible, but it is not yet supported in orpc.)

When any exceptions are declared in the input, then the calling
convention for the entire interface is changed: each return value is
wrapped in an `'a orpc_result`, defined as
`Orpc_success of 'a | Orpc_failure of exn`,
to accomodate the possibility of exception. This doesn't affect the
external interface, but means the wire protocol is not compatible.

An exception raised on the server which is one of those declared in
the input is marshalled and raised on the client. Others are reported
as a generic transport-level exception in the usual way.
