//// This module provides functions to work with `OSet`s

import bravo/error.{type ErlangError}
import bravo/etc.{type Access}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/erlang/atom.{type Atom}
import gleam/option.{type Option, None, Some}
import gleam/result

/// An ordered set. Keys may only occur once per table, and objects are ordered (this comes at a performance cost).
///
/// In order for a lookup match to occur, entries must _coerce into the same value_. Two values may match even if they have different types.
///
pub opaque type OSet(t) {
  OSet(table: Atom, keypos: Int)
}

/// Creates a new ETS table configured as an ordered set: keys may only occur once per table, and objects are ordered (this comes at a performance cost).
///
/// `name`: An atom representing the name of the `OSet`. There may only be one ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `OSet`.
/// - `Protected`: Any process can read the `OSet`. Only the owner process can write to it.
/// - `Private`: Only the parent process can read or write to the `OSet`.
///
/// Returns a result of the created `OSet`, which can be used by other functions in this module.
/// If this function errors with `None`, then you likely put in an illegal `keypos` value.
/// Otherwise, something went wrong in the FFI layer and an error occured in Erlang land.
///
pub fn new(
  name: String,
  keypos: Int,
  access: Access,
) -> Result(OSet(t), Option(ErlangError)) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(None))
  use a <- result.try(
    bindings.new(atom, [
      new_option.OrderedSet,
      case access {
        etc.Public -> new_option.Public
        etc.Protected -> new_option.Protected
        etc.Private -> new_option.Private
      },
      new_option.NamedTable,
      new_option.Keypos(keypos),
      new_option.WriteConcurrency(new_option.Auto),
      new_option.ReadConcurrency(True),
      new_option.DecentralizedCounters(True),
    ])
    |> result.map_error(fn(e) { Some(e) }),
  )
  Ok(OSet(a, keypos))
}

/// Inserts a list of tuples into a `OSet`.
///
/// Returns a `Bool` representing if the inserting succeeded. 
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the size of the tuple is less than the `OSet`'s size.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(oset: OSet(t), objects: List(t)) -> Bool {
  bindings.try_insert(oset.table, oset.keypos, objects)
}

/// Gets an object from a `OSet`.
///
/// Returns an `Option` containing the object, if it exists.
///
pub fn lookup(oset: OSet(t), key: a) -> Option(t) {
  case bindings.try_lookup(oset.table, key) {
    [res] -> Some(res)
    _ -> None
  }
}

/// Deletes a `OSet`.
///
/// Table lifetime is static, and memory is only freed when the owner process is killed! Don't forget to call this function!
///
pub fn delete(oset: OSet(t)) -> Bool {
  bindings.try_delete(oset.table)
}
