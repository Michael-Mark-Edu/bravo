# bravo
### v1.0.0

[![Package Version](https://img.shields.io/hexpm/v/bravo)](https://hex.pm/packages/bravo)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/bravo/)

~~Comprehensive~~ Very much incomplete (for now) ETS bindings for Gleam.

This library is still in heavy development! Expect frequent breaking changes (this is indicated
when the first number of the version increases).

This library is only compatible with the Erlang target!

## What is an ETS?

ETS stands for Erlang Term Storage, and it is a data structure that stores collections of data
(objects) that are addressable by keys.

### Why should I use ETS over, say, a `Dict`?

ETS tables have the following properties that make them distinct from `Dict`s:
- ETS tables are not garbage collected, allowing them to be accessed from anywhere in a process
  without needing to pass return values around. This does come at the cost of forcing the user
  to manually delete the table once they are done with it.
- ETS tables offer a wide array of customization options that `Dict`s do not, such as the options
  to be ordered, allow multiple objects per key, which element of the object acts as the key,
  and controlling read/write access from external processes.
- ETS tables offer increased functionality compared to `Dict`s, natively allowing functions such as 
  complex searching, reading/writing the ETS to a file, and having much more complex entries.
- ETS tables work well with concurrency.

### Why should I use Bravo over other libraries such as Carpenter?

A difficulty of implementing ETS tables in Gleam is that Gleam is a statically-typed language and
ETS tables are very much designed for the dynamically-typed Erlang. Carpenter ensures type safety
by restricting objects to simple key-value pairs, much like a `Dict`.

Bravo, however, sacrifices type guarantees to empower the ETS table. ETS tables do not store type
information, so the burden of interpreting the binary data stored in the ETS into a usable Gleam
type is placed on the user. Fortunately, the `gleam/dynamic` library provides powerful tools to
work around this. As long as you know what each object's type signature is, you should be fine.

## Installation
Bravo depends on the `gleam_erlang` package, but otherwise does not have any other dependency.

```sh
gleam add bravo gleam_erlang
```

## Usage
```gleam
import bravo/object
import bravo/table
import bravo/uset
import gleam/dynamic
import gleam/erlang/atom
import gleam/io
import gleam/option.{Some}

pub fn main() {
  // Create a new ETS table. There are multiple options, but here we are using
  // a USet (an alias for "set" in Erlang terms)
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 1, table.Public)
  // We can then insert tuples into this table
  uset.insert(table, [#("Hello", "world!")])
  // Then we can lookup the object from the table
  let assert Some(object) = uset.lookup(table, "Hello")
  let assert Ok(tuple) =
    object
    |> object.extract
    |> dynamic.tuple2(dynamic.string, dynamic.string)
  io.print(tuple.0 <> ", " <> tuple.1) // "Hello, world!"
  // ETS tables have static lifetimes,
  // so don't forget to delete them when you're done!
  uset.delete(table)
}
```

Further documentation can be found at <https://hexdocs.pm/bravo>.
