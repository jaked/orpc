#summary Using orpc with RPC over HTTP with ocamljs

Orpc can also be used to call over HTTP from Javascript clients
written using [http://code.google.com/p/ocamljs/ ocamljs] to Ocamlnet
servers. When calling `orpc` use the `--js` argument to generate this
kind of client / server.

The simple style of interface is not supported in this mode. The only
supported module kind on the server is Sync, since it uses Ocamlnet's
`netcgi`; on the client you can use Lwt or Async (Sync is possible but
you'll hang the UI while a request is waiting).

To link the server use the `orpc-js-server` package; for the client
use `orpc-js-client`.

For more details see the `clicks` example.
