# bravo
### [v3.0.0-rc.1 (This is a breaking update!)](https://github.com/Michael-Mark-Edu/bravo/blob/main/CHANGELOG.md#v300)

[![Package Version](https://img.shields.io/hexpm/v/bravo)](https://hex.pm/packages/bravo)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/bravo/)

Comprehensive ETS bindings for Gleam.

This library is only compatible with the Erlang target!

## What is an ETS?

ETS stands for Erlang Term Storage, and it is a data structure that stores collections of data
(objects) that are addressable by keys.

### Why should I use ETS over, say, a `Dict`?

ETS tables have the following properties that make them distinct from `Dict`s:
- ETS tables are inherently mutable, allowing for faster operations especially when working with
  large amounts of data.
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

Bravo, however, does not impose much of any restriction at all. The only restriction that is in
place is that all objects in a table must have the same type, but that type can be literally
anything. In an earlier version of this package, even this wasn't a restriction, but that resulted
in unsafe and un-Gleamlike code.

A goal of Bravo is to be fully comprehensive, meaning that I intend on implementing every single
ETS function into the library. This goal may very well be infeasible, but I will pursue it anyway.

## Installation
Bravo depends on the `gleam_erlang` package, but otherwise does not have any other dependency.

```sh
gleam add bravo gleam_erlang
```

## Example
```gleam
import bravo
import bravo/uset
import gleam/io
import gleam/option.{Some}

pub fn main() {
  // Create a new ETS table. There are multiple options, but here we are using
  // a USet (an alias for "set" in Erlang terms)
  let assert Ok(table) = uset.new("MyTable", 1, bravo.Public)

  // We can then insert tuples into this table
  uset.insert(table, [#("Hello", "world!")])

  // Then we can lookup the object from the table
  let assert Some(object) = uset.lookup(table, "Hello")
  io.print(object.0 <> ", " <> object.1) // "Hello, world!"

  // ETS tables have static lifetimes,
  // so don't forget to delete them when you're done!
  uset.delete(table)
}
```

## Bravo in Five Minutes

First, you must choose a table type. Bravo offers four options:

- `USet` (set): Keys may only occur once per table, and objects are unordered.
- `OSet` (ordered set): Keys may only occur once per table, and objects are ordered (this comes at a performance cost).
- `Bag` (bag): Keys may occur multiple times per table, but objects cannot be copied verbatim.
- `DBag` (duplicate bag): Keys may occur multiple times per table, and verbatim copies of an object can be stored.

For the sake of demonstration, this section will use the `USet`.

First, we must create a `USet`. We must pass in a name, the table's `keypos` (recommended to leave
this as 1) and a access specifier (not relevant unless you're working with processes/actors).
```gleam
import bravo
import bravo/uset

let assert Ok(table) = uset.new("MyTable", 1, bravo.Public)
```
This will return a `Result` containing the table, if the table creation succeeded. If there
already exists a table with the same name, this function will fail.

This table will be empty, so we'll need to insert some _objects_ into it. An object can be of any
type, but it should ideally be a tuple (due to how Bravo handles non-tuples, it's more memory
efficient to use a size-1 tuple instead of a non-tuple, but this may change in the future).

To insert, we use the `insert` function, which takes in a list of objects.
```gleam
uset.insert(table, [#("Hello", "World")])
```

_A table can only accept new objects that are the same type!_ This is enforced at compile time,
and the type of the table is determined via type inference through the first `insert` call. (Tip:
if you need a table with multiple object types, it is valid to use `dynamic.from` in the `insert`
call to do this, but it introduces unsafety into your code that must be resolved using decoders.)

Now that we have an object in the table, we can look it up using the `lookup` function. It
simply takes in the table and a key (in this case, the first element of the tuple), and returns the
object (or list of objects in the case of `Bag` and `DBag`) if it was found.
```gleam
uset.lookup(table, "Hello")
|> should.equal(Some(#("Hello", "World")))
```

Theoretically, this is all you need to be able to use Bravo, but there are more functions that
may be useful:
- `delete`: Completely deletes a table
- `delete_key`: Deletes all objects with the key
- `insert_new`: Inserts an object(s), but only if there does not exist any object with the same key
- `take`: Looks up an object, returns it, then removes the object from the table
- `member`: Gets if a table contains at least one object with the given key
- `tab2file`: Saves the ETS table to a file.
- `file2tab`: Reverses the above function. Due to type safety, this needs a dynamic decoder.

ETS tables are powerful tools provided natively by Erlang and the Beam VM, so make use of them!

Further documentation can be found at <https://hexdocs.pm/bravo>.
