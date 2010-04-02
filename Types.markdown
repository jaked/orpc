---
layout: page
title: Supported types
---
#Supported types

The following OCaml base types are supported:

| OCaml type | XDR type      |
|------------|---------------|
| `unit`     | `void`        |
| `int`      | `int`         |
| `int32`    | `int`         |
| `int64`    | `hyper`       |
| `float`    | `double`      |
| `bool`     | `bool`        |
| `char`     | `enum 0..255` |
| `string`   | `string<>`    |

Also supported are built-in compound types (`[['a]]` is the XDR
equivalent of `'a`):

| OCaml type          | XDR type                                                    |
|---------------------|-------------------------------------------------------------|
| `'a * 'b * ...`     | `struct { [['a]] 0; [['b]] 1; ... }`                        |
| `{ foo : 'a; ... }` | `struct { [['a]] foo; ... }`                                |
| `Foo of 'a ...`     | `union switch () { case 0: [['a]] 0; ... }`                 |
| `` `foo of 'a ...`` | `union switch () { case 0: [['a]] 0; ... }`                 |
| `'a array`          | `[['a]]<>`                                                  |
| `'a list`           | `union switch () { case 0: void 0; case 1: [['a list]] 1 }` |
| `'a option`         | `[['a]] *`                                                  |
| `'a ref`            | `[['a]]`                                                    |

Finally, new types may be defined in the usual way, possibly with type
parameters. However, a parameterized type must be instantiated when
used in a remote function declaration (see [Functions] (Functions.html)).
The XDR equivalent of an instance of a polymorphic type
is just the (XDR equivalent of the) type with the actual types
substituted in.

Type equations are permitted; you must also give the definition of the
type so orpc can generate marshalling functions for it.

Mutable record fields and refs are permitted, although of course
changing a field on one end of a network connection does not change it
on the other.
