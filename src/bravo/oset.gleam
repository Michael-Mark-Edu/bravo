//// This module provides functions to work with `OSet`s

import bravo.{type Access, type BravoError}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

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
/// Can have error types `ErlangError` and `NonPositiveKeypos`.
///
pub fn new(
  name: String,
  keypos: Int,
  access: Access,
) -> Result(OSet(t), BravoError) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(bravo.NonPositiveKeypos))
  use a <- result.try(
    bindings.try_new(atom, [
      new_option.OrderedSet,
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
  Ok(OSet(a, keypos))
}

/// Inserts a list of tuples into a `OSet`.
///
/// Returns a `Bool` representing if the inserting succeeded.
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the `keypos` of the `OSet` is greater than the object tuple size or if the input list is empty.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(oset: OSet(t), objects: List(t)) -> Bool {
  use <- bool.guard(list.is_empty(objects), False)
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

/// Deletes the object addressed by `key`, if it exists. If it doesn't, this does nothing.
///
pub fn delete_key(oset: OSet(t), key: a) -> Nil {
  bindings.try_delete_key(oset.table, key)
  Nil
}

/// Deletes all objects in the `OSet`. This is atomic and isolated.
///
pub fn delete_all_objects(oset: OSet(t)) -> Nil {
  bindings.try_delete_all_objects(oset.table)
  Nil
}

/// Deletes a specific object in the `OSet`. This is more useful in `Bag`s and `DBag`s.
///
pub fn delete_object(oset: OSet(t), object: t) -> Nil {
  bindings.try_delete_object(oset.table, object)
  Nil
}

/// Saves a `OSet` as file `filename` that can later be read back into memory using `file2tab`.
///
/// There are three configuration flags with this function:
/// - `object_count`: Stores the number of objects in the table in the footer. This can detect truncation.
/// - `md5sum`: Stores a md5 checksum of the table and its objects. This can detect even single bitflips, but is computationally expensive.
/// - `sync`: Blocks the process until the file has been successfully written.
///
/// Can have error type `ErlangError`.
///
pub fn tab2file(
  oset: OSet(t),
  filename: String,
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Result(Nil, BravoError) {
  bindings.try_tab2file(
    oset.table,
    string.to_utf_codepoints(filename),
    object_count,
    md5sum,
    sync,
  )
}

/// Creates a `OSet` from file `filename` that was previously created by `tab2file`.
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
) -> Result(OSet(t), BravoError) {
  use name <- result.try(bindings.try_file2tab(
    string.to_utf_codepoints(filename),
    verify,
  ))
  let assert Ok(keypos) =
    dynamic.int(bindings.inform(name, atom.create_from_string("keypos")))
  let table = OSet(name, keypos)
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

/// Returns a list containing all of the objects in the `OSet`.
///
/// The list returned is ordered.
///
pub fn tab2list(oset: OSet(t)) -> List(t) {
  bindings.try_tab2list(oset.table)
}

/// Inserts a list of tuples into a `OSet`. Unlike `insert`, this cannot overwrite objects and will return false if it tries to do so.
///
/// Returns a `Bool` representing if the inserting succeeded.
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the `keypos` of the `OSet` is greater than the object tuple size or if the input list is empty.
///
pub fn insert_new(oset: OSet(t), objects: List(t)) -> Bool {
  use <- bool.guard(list.is_empty(objects), False)
  bindings.try_insert_new(oset.table, oset.keypos, objects)
}

/// Returns and removes an object at `key` in the `OSet`, if such object exists.
///
pub fn take(oset: OSet(t), key: a) -> Option(t) {
  case bindings.try_take(oset.table, key) {
    [res] -> Some(res)
    _ -> None
  }
}

/// Returns whether a `OSet` contains an object at `key`.
///
pub fn member(oset: OSet(t), key: a) -> Bool {
  bindings.try_member(oset.table, key)
}
