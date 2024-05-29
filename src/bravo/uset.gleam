//// This module provides functions to work with `USet`s

import bravo/error.{type ErlangError}
import bravo/etc.{type Access}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/erlang/atom.{type Atom}
import gleam/option.{type Option, None, Some}
import gleam/result

/// An unordered set. Keys may only occur once per table, and objects are unordered.
///
/// In order for a lookup match to occur, entries must have the same value _and type_.
///
pub opaque type USet(t) {
  USet(table: Atom, keypos: Int)
}

/// Creates a new ETS table configured as a set: keys may only occur once per table, and objects are unordered.
///
/// `name`: An atom representing the name of the `USet`. There may only be one ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `USet`.
/// - `Protected`: Any process can read the `USet`. Only the owner process can write to it.
/// - `Private`: Only the parent process can read or write to the `USet`.
///
/// Returns a result of the created `USet`, which can be used by other functions in this module.
/// If this function errors with `None`, then you likely put in an illegal `keypos` value.
/// Otherwise, something went wrong in the FFI layer and an error occured in Erlang land.
///
pub fn new(
  name: String,
  keypos: Int,
  access: Access,
) -> Result(USet(t), Option(ErlangError)) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(None))
  use a <- result.try(
    bindings.new(atom, [
      new_option.Set,
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
  Ok(USet(a, keypos))
}

/// Inserts a list of tuples into a `USet`.
///
/// Returns a `Bool` representing if the inserting succeeded. 
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the size of the tuple is less than the `USet`'s size.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(uset: USet(t), objects: List(t)) -> Bool {
  bindings.try_insert(uset.table, uset.keypos, objects)
}

/// Gets an object from a `USet`.
///
/// Returns an `Option` containing the object, if it exists.
///
pub fn lookup(uset: USet(t), key: a) -> Option(t) {
  case bindings.try_lookup(uset.table, key) {
    [res] -> Some(res)
    _ -> None
  }
}

/// Deletes a `USet`.
///
/// Table lifetime is static, and memory is only freed when the owner process is killed! Don't forget to call this function!
///
pub fn delete(uset: USet(t)) -> Bool {
  bindings.try_delete(uset.table)
}

/// Deletes the object addressed by `key`, if it exists. If it doesn't, this does nothing.
///
pub fn delete_key(uset: USet(t), key: a) -> Nil {
  bindings.try_delete_key(uset.table, key)
  Nil
}

/// Deletes all objects in the `USet`. This is atomic and isolated.
///
pub fn delete_all_objects(uset: USet(t)) -> Nil {
  bindings.try_delete_all_objects(uset.table)
  Nil
}

/// Deletes a specific object in the `USet`. This is more useful in `Bag`s and `DBag`s.
///
pub fn delete_object(uset: USet(t), object: t) -> Nil {
  bindings.try_delete_object(uset.table, object)
  Nil
}
