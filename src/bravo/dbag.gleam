//// This module provides functions to work with `DBag`s
////

import bravo/error.{type ErlangError}
import bravo/etc.{type Access}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

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
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the `keypos` of the `DBag` is greater than the object tuple size or if the input list is empty.
///
/// If an object with the same key already exists, then the old object will be overwritten with the new one.
///
pub fn insert(dbag: DBag(t), objects: List(t)) -> Bool {
  use <- bool.guard(list.is_empty(objects), False)
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

/// Deletes all objects addressed by `key`, if any exist. If nothing does, this does nothing.
///
pub fn delete_key(dbag: DBag(t), key: a) -> Nil {
  bindings.try_delete_key(dbag.table, key)
  Nil
}

/// Deletes all objects in the `DBag`. This is atomic and isolated.
///
pub fn delete_all_objects(dbag: DBag(t)) -> Nil {
  bindings.try_delete_all_objects(dbag.table)
  Nil
}

/// Deletes a specific object in the `DBag`. Other objects with the same key are unaffected.
///
/// If there are multiple of the same object, then they will all be deleted.
///
pub fn delete_object(dbag: DBag(t), object: t) -> Nil {
  bindings.try_delete_object(dbag.table, object)
  Nil
}

/// Saves a `Ddbag` as file `filename` that can later be read back into memory using `file2tab`.
///
/// There are three configuration flags with this function:
/// - `object_count`: Stores the number of objects in the table in the footer. This can detect truncation.
/// - `md5sum`: Stores a md5 checksum of the table and its objects. This can detect even single bitflips, but is computationally expensive.
/// - `sync`: Blocks the process until the file has been successfully written.
///
pub fn tab2file(
  dbag: DBag(t),
  filename: String,
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Bool {
  case
    bindings.try_tab2file(
      dbag.table,
      string.to_utf_codepoints(filename),
      object_count,
      md5sum,
      sync,
    )
  {
    new_option.Ok -> True
    new_option.Error(_) -> False
  }
}

/// Creates a `Ddbag` from file `filename` that was previously created by `tab2file`.
///
/// For type safety reasons, a dynamic decoder must be provided, and the decoder must not fail for all objects in the table.
///
/// If the flag `verify` is set, then checks are performed to ensure the data is correct. This can be slow if `tab2file` was ran with `md5sum` enabled.
///
pub fn file2tab(
  filename: String,
  verify: Bool,
  decoder: fn(Dynamic) -> Result(t, _),
) -> Option(DBag(t)) {
  case bindings.try_file2tab(string.to_utf_codepoints(filename), verify) {
    Error(_) -> None
    Ok(name) -> {
      let assert Ok(keypos) =
        dynamic.int(bindings.inform(name, atom.create_from_string("keypos")))
      let table = DBag(name, keypos)
      use <- bool.guard(
        !{
          use obj <- list.all(tab2list(table))
          case decoder(dynamic.from(obj)) {
            Ok(_) -> True
            Error(_) -> False
          }
        },
        None,
      )
      Some(table)
    }
  }
}

/// Returns a list containing all of the objects in the `Ddbag`.
///
pub fn tab2list(dbag: DBag(t)) -> List(t) {
  bindings.try_tab2list(dbag.table)
}
