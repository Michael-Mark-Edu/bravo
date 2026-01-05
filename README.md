# bravo
### [Changelog](https://github.com/Michael-Mark-Edu/bravo/blob/main/CHANGELOG.md)

[![Package Version](https://img.shields.io/hexpm/v/bravo)](https://hex.pm/packages/bravo)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/bravo/)

Comprehensive ETS bindings for Gleam.

This library is only compatible with the Erlang target!

## What is an ETS?

ETS stands for Erlang Term Storage, and it is a data structure that stores collections of data
(objects) that are addressable by keys. They are useful for static-lifetime storage, in-memory
caches, or even simple NoSQL DBMSes.

### Why should I use ETS over, say, a `Dict`?

ETS tables have the following properties that make them distinct from `Dict`s:
- ETS tables are inherently mutable, allowing for better performance in write-heavy situations.
- ETS tables are not garbage collected, allowing them to be accessed from anywhere in a process
  without needing to pass return values around. This does come at the cost of forcing the user
  to manually delete the table once they are done with it (assuming the table isn't supposed to
  exist for the lifetime of the program).
- ETS tables offer a wide array of customization options that `Dict`s do not, such as setting
  orderedness and controlling external process read/write access.
- ETS tables offer increased functionality compared to `Dict`s, natively allowing functions such as
  complex searching and reading/writing the ETS to a file.
- ETS tables function well in parallel environments, with settings available to control how the
  table behaves concurrently.

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

pub fn main() {
  // Create a new ETS table. There are multiple options, but here we are using
  // a USet (an alias for "set" in Erlang terms)
  let assert Ok(table) = uset.new("MyTable", bravo.Public)

  // We can then insert key-value pairs into this table
  let assert Ok(Nil) = uset.insert(table, "Hello", "world!")

  // Then we can lookup the value from the table using the key
  let assert Ok(value) = uset.lookup(table, "Hello")
  io.println(value) // "world!"

  // ETS tables have static lifetimes.
  // If a table should only exist for a limited period of time, then delete it!
  let assert Ok(Nil) = uset.delete(table)
}
```

## Bravo in Five Minutes

First, you must choose a table type. Bravo offers four options:

- `USet` (set): Keys may only occur once per table, and objects are unordered.
- `OSet` (ordered set): Keys may only occur once per table, and objects are ordered (this comes at a performance cost).
- `Bag` (bag): Keys may occur multiple times per table, but objects cannot be copied verbatim.
- `DBag` (duplicate bag): Keys may occur multiple times per table, and verbatim copies of an object can be stored.

For the sake of demonstration, this section will use the `USet`.

First, we must create a `USet`. We must pass in a name and an access specifier (not relevant unless
you're working with processes/actors).

```gleam
import bravo
import bravo/uset

let assert Ok(table) = uset.new("MyTable", bravo.Public)
```
This will return a `Result` containing the table, if the table creation succeeded. If there
already exists a table with the same name, this function will fail.

Values are stored as key-value pairs. To insert an object, use the `insert` function and specify
the key and value to store.

```gleam
let assert Ok(Nil) = uset.insert(table, "Hello", "world!")
```

_A table can only accept new objects that are the same type!_ The key or value types can be
`Dynamic`s if you need heterogeneity, or tuples if storing just one value per key is not enough (or
even have the key be a tuple as a sort of composite key if you want). This restriction is enforced
at compile time, and the type of the table is determined via type inference (typically the first
`insert` call will set the types).

Now that we have an object in the table, we can look it up using the `lookup` function. It
simply takes in the table and a key (in this case, the first element of the tuple), and returns the
object (or list of objects in the case of `Bag` and `DBag`) if it was found.

```gleam
uset.lookup(table, "Hello")
|> should.equal(Ok("world!"))
```

Theoretically, this is all you need to be able to use Bravo, but there are more functions that
may be useful. Here's a few (see docs for full list):
- `delete`: Completely deletes a table
- `delete_key`: Deletes all objects with the key
- `insert_new`: Inserts an object, but only if there does not exist any object with the same key
- `take`: Looks up an object, returns it, then removes the object from the table
- `member`: Gets if a table contains at least one object with the given key
- `tab2file`: Saves the ETS table to a file.
- `file2tab`: Reverses the above function. Due to type safety, this needs a dynamic decoder.

ETS tables are powerful tools provided natively by Erlang and the Beam VM, so make use of them!

Further documentation can be found at <https://hexdocs.pm/bravo>.
