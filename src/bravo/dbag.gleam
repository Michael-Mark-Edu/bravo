//// This module provides functions to work with `DBag`s

import bravo/error.{type ErlangError}
import bravo/etc.{type Access}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/erlang/atom.{type Atom}
import gleam/option.{type Option, None, Some}
import gleam/result

/// A duplicate bag etc. Keys may occur multiple times per table, and verbatim copies of an object can be stored.
///
pub opaque type DBag(t) {
  DBag(table: Atom, keypos: Int)
}

/// Creates a new ETS table configured as a duplicate bag: keys may occur multiple times per table, and verbatim copies of an object can be stored.
///
/// `name`: An atom representing the name of the `DBag`. There may only be one ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `DBag`.
/// - `Protected`: Any process can read the `DBag`. Only the owner process can write to it.
/// - `Private`: Only the parent process can read or write to the `DBag`.
///
/// Returns a result of the created `DBag`, which can be used by other functions in this module.
/// If this function errors with `None`, then you likely put in an illegal `keypos` value.
/// Otherwise, something went wrong in the FFI layer and an error occured in Erlang land.
///
pub fn new(
  name: String,
  keypos: Int,
  access: Access,
) -> Result(DBag(t), Option(ErlangError)) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(None))
  use a <- result.try(
    bindings.new(atom, [
      new_option.DuplicateBag,
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
  Ok(DBag(a, keypos))
}

/// Inserts a list of tuples into a `DBag`.
///
/// Returns a `Bool` representing if the inserting succeeded. 
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the size of the tuple is less than the `DBag`'s size.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(dbag: DBag(t), objects: List(t)) -> Bool {
  bindings.try_insert(dbag.table, dbag.keypos, objects)
}

/// Gets a list of objects from a `DBag`.
///
/// Returns an list containing the objects, if any match.
///
pub fn lookup(dbag: DBag(t), key: a) -> List(t) {
  bindings.try_lookup(dbag.table, key)
}

/// Deletes a `DBag`.
///
/// Table lifetime is static, and memory is only freed when the owner process is killed! Don't forget to call this function!
///
pub fn delete(dbag: DBag(t)) -> Bool {
  bindings.try_delete(dbag.table)
}
