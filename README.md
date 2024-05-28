# bravo

[![Package Version](https://img.shields.io/hexpm/v/bravo)](https://hex.pm/packages/bravo)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/bravo/)

Comprehensive ETS bindings for Gleam.

This library is only compatible with the Erlang target!

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
