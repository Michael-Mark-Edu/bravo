//// This module provides functions to work with `Bag`s

import bravo.{type Access, type BravoError}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

/// A bag table. Keys may occur multiple times per table, but objects cannot be copied verbatim.
///
pub opaque type Bag(t) {
  Bag(table: Atom, keypos: Int)
}

/// Creates a new ETS table configured as a bag: keys may only occur multiple times per table, but objects cannot be copied verbatim.
///
/// `name`: An atom representing the name of the `Bag`. There may only be one ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `Bag`.
/// - `Protected`: Any process can read the `Bag`. Only the owner process can write to it.
/// - `Private`: Only the parent process can read or write to the `Bag`.
///
/// Returns a result of the created `Bag`, which can be used by other functions in this module.
/// Can have error types `ErlangError` and `NonPositiveKeypos`.
///
pub fn new(
  name: String,
  keypos: Int,
  access: Access,
) -> Result(Bag(t), BravoError) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(bravo.NonPositiveKeypos))
  use a <- result.try(
    bindings.try_new(atom, [
      new_option.Bag,
      case access {
        bravo.Public -> new_option.Public
        bravo.Protected -> new_option.Protected
        bravo.Private -> new_option.Private
      },
      new_option.NamedTable,
      new_option.Keypos(keypos),
      new_option.WriteConcurrency(new_option.Auto),
      new_option.ReadConcurrency(True),
      new_option.DecentralizedCounters(True),
    ]),
  )
  Ok(Bag(a, keypos))
}

/// Inserts a list of tuples into a `Bag`.
///
/// Returns a `Bool` representing if the inserting succeeded.
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the `keypos` of the `Bag` is greater than the object tuple size or if the input list is empty.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(bag: Bag(t), objects: List(t)) -> Bool {
  use <- bool.guard(list.is_empty(objects), False)
  bindings.try_insert(bag.table, bag.keypos, objects)
}

/// Gets a list of objects from a `Bag`.
///
/// Returns an list containing the objects, if any match.
///
pub fn lookup(bag: Bag(t), key: a) -> List(t) {
  bindings.try_lookup(bag.table, key)
}

/// Deletes a `Bag`.
///
/// Table lifetime is static, and memory is only freed when the owner process is killed! Don't forget to call this function!
///
pub fn delete(bag: Bag(t)) -> Bool {
  bindings.try_delete(bag.table)
}

/// Deletes all objects addressed by `key`, if any exist. If nothing does, this does nothing.
///
pub fn delete_key(bag: Bag(t), key: a) -> Nil {
  bindings.try_delete_key(bag.table, key)
  Nil
}

/// Deletes all objects in the `Bag`. This is atomic and isolated.
///
pub fn delete_all_objects(bag: Bag(t)) -> Nil {
  bindings.try_delete_all_objects(bag.table)
  Nil
}

/// Deletes a specific object in the `Bag`. Other objects with the same key are unaffected.
///
pub fn delete_object(bag: Bag(t), object: t) -> Nil {
  bindings.try_delete_object(bag.table, object)
  Nil
}

/// Saves a `Bag` as file `filename` that can later be read back into memory using `file2tab`.
///
/// There are three configuration flags with this function:
/// - `object_count`: Stores the number of objects in the table in the footer. This can detect truncation.
/// - `md5sum`: Stores a md5 checksum of the table and its objects. This can detect even single bitflips, but is computationally expensive.
/// - `sync`: Blocks the process until the file has been successfully written.
///
/// Can have error type `ErlangError`.
///
pub fn tab2file(
  bag: Bag(t),
  filename: String,
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Result(Nil, BravoError) {
  bindings.try_tab2file(
    bag.table,
    string.to_utf_codepoints(filename),
    object_count,
    md5sum,
    sync,
  )
}

/// Creates a `Bag` from file `filename` that was previously created by `tab2file`.
///
/// For type safety reasons, a dynamic decoder must be provided, and the decoder must not fail for all objects in the table.
///
/// If the flag `verify` is set, then checks are performed to ensure the data is correct. This can be slow if `tab2file` was ran with `md5sum` enabled.
///
/// Size-1 tuples are handled uniquely internally and are treated as non-tuples when saved to disk, so you must decode the non-tuple version of the type.
///
/// Can have error types `DecodeFailure` and `ErlangError`.
///
pub fn file2tab(
  filename: String,
  verify: Bool,
  decoder: fn(Dynamic) -> Result(t, _),
) -> Result(Bag(t), BravoError) {
  use name <- result.try(bindings.try_file2tab(
    string.to_utf_codepoints(filename),
    verify,
  ))
  let assert Ok(keypos) =
    dynamic.int(bindings.inform(name, atom.create_from_string("keypos")))
  let table = Bag(name, keypos)
  list.map(tab2list(table), fn(obj: t) {
    delete_object(table, obj)
    case bindings.tuple_size(obj) {
      1 -> insert(table, [bindings.element(1, obj)])
      _ -> insert(table, [obj])
    }
  })
  use <- bool.guard(
    {
      use obj <- list.all(tab2list(table))
      obj
      |> dynamic.from
      |> decoder
      |> result.is_ok
    },
    Ok(table),
  )
  delete(table)
  Error(bravo.DecodeFailure)
}

/// Returns a list containing all of the objects in the `Bag`.
///
pub fn tab2list(bag: Bag(t)) -> List(t) {
  bindings.try_tab2list(bag.table)
}

/// Inserts a list of tuples into a `Bag`. Unlike `insert`, this cannot overwrite objects and will return false if it tries to do so.
///
/// Returns a `Bool` representing if the inserting succeeded.
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the `keypos` of the `Bag` is greater than the object tuple size or if the input list is empty.
///
pub fn insert_new(bag: Bag(t), objects: List(t)) -> Bool {
  use <- bool.guard(list.is_empty(objects), False)
  bindings.try_insert_new(bag.table, bag.keypos, objects)
}

/// Returns and removes all objects with `key` in the `Bag`, if any exist.
///
pub fn take(bag: Bag(t), key: a) -> List(t) {
  bindings.try_take(bag.table, key)
}

/// Returns whether a `Bag` contains an object at `key`.
///
pub fn member(bag: Bag(t), key: a) -> Bool {
  bindings.try_member(bag.table, key)
}

pub fn first(bag: Bag(t)) -> Option(a) {
  bindings.try_first(bag.table)
}

pub fn last(bag: Bag(t)) -> Option(a) {
  bindings.try_last(bag.table)
}

pub fn next(bag: Bag(t), key: a) -> Option(a) {
  bindings.try_next(bag.table, key)
}

pub fn prev(bag: Bag(t), key: a) -> Option(a) {
  bindings.try_prev(bag.table, key)
}
