//// This module provides functions to work with `OSet`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_option
import gleam/dynamic.{type Dynamic}
import gleam/result

/// An ordered set. Keys may only occur once per table, and keys are ordered
/// (this comes at a performance cost).
///
/// The specific order of keys is based on the [Erlang documentation.](https://www.erlang.org/doc/system/expressions.html#term-comparisons)
///
/// In order for a lookup match to occur, entries must _coerce into the
/// same value_. Two values may match even if they have different types.
pub opaque type OSet(t) {
  OSet(inner: master.InnerTable)
}

/// Creates a new ETS table configured as an ordered set: keys may only occur
/// once per table, and objects are ordered (this comes at a performance cost).
///
/// `name`: An atom representing the name of the `OSet`. There may only be one
/// ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the
/// object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `OSet`.
/// - `Protected`: Any process can read the `OSet`. Only the owner process can
///   write to it.
/// - `Private`: Only the parent process can read or write to the `OSet`.
///
/// Returns a result of the created `OSet`, which can be used by other functions
/// in this module.
///
/// Can have error types `ErlangError` and `NonPositiveKeypos`.
pub fn new(
  name name: String,
  keypos keypos: Int,
  access access: Access,
) -> Result(OSet(t), BravoError) {
  use res <- result.try(master.new(name, keypos, access, new_option.OrderedSet))
  Ok(OSet(res))
}

/// Inserts a list of tuples into a `OSet`.
///
/// If the insertion was successful, returns `Ok(Nil)`. Othwerise:
/// - `Error(InvalidKeypos)`: The size of the inserted tuple is less than the
///   `keypos` of the table (or in the case of a non-tuple, `keypos` is greater
///   than 1).
/// - `Error(TableDoesNotExist)`: The table was either deleted or never created.
/// - `Error(AccessDenied)`: The table has an access level of `Protected` or
///   `Private` and is owned by a different process.
/// - `Error(NothingToInsert)`: Parameter `insert` is an empty list.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
///
/// If an object with the same key already exists, then the old object will be
/// overwritten with the new one. To instead get an error, use `insert_new`.
pub fn insert(
  with oset: OSet(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert(oset.inner, objects)
}

/// Inserts a list of tuples into a `OSet`. Unlike `insert`, this cannot
/// overwrite objects and will return false if it tries to do so.
///
/// If the insertion was successful, returns `Ok(Nil)`. Othwerise:
/// - `Error(InvalidKeypos)`: The size of the inserted tuple is less than the
///   `keypos` of the table (or in the case of a non-tuple, `keypos` is greater
///   than 1).
/// - `Error(TableDoesNotExist)`: The table was either deleted or never created.
/// - `Error(AccessDenied)`: The table has an access level of `Protected` or
///   `Private` and is owned by a different process.
/// - `Error(NothingToInsert)`: Parameter `insert` is an empty list.
/// - `Error(KeyAlreadyPresent)`: At least one inserted object's key matches
///   with the key of an already present object.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
///
/// If an object with the same key already exists, then the old object will be
/// overwritten with the new one. To instead overwrite, use `insert`.
pub fn insert_new(
  with oset: OSet(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert_new(oset.inner, objects)
}

/// Gets an object from a `OSet`.
///
/// Returns an `Result` containing the object, if it exists.
pub fn lookup(with oset: OSet(t), at key: a) -> Result(t, Nil) {
  master.lookup_set(oset.inner, key)
}

/// Deletes a `OSet`.
///
/// Table lifetime is static, and memory is only freed when the owner process is
/// killed! Don't forget to call this function if you intend for this table to
/// be temporary!
///
/// The input `OSet` is completely useless after it is deleted. Even if another
/// table is created with the same name, the old handle will not work.
pub fn delete(with oset: OSet(t)) -> Bool {
  master.delete(oset.inner)
}

/// Deletes the object addressed by `key`, if it exists. If it doesn't, this
/// does nothing.
pub fn delete_key(with oset: OSet(t), at key: a) -> Nil {
  master.delete_key(oset.inner, key)
}

/// Deletes all objects in the `OSet`. This is atomic and isolated.
pub fn delete_all_objects(with oset: OSet(t)) -> Nil {
  master.delete_all_objects(oset.inner)
}

/// Deletes a specific object in the `OSet`. This is more useful with
/// `oset`s and `Doset`s.
pub fn delete_object(with oset: OSet(t), target object: t) -> Nil {
  master.delete_object(oset.inner, object)
}

/// Saves a `OSet` as file `filename` that can later be read back into memory
/// using `file2tab`.
///
/// There are three configuration flags with this function:
/// - `object_count`: Stores the number of objects in the table in the footer.
///   This can detect truncation.
/// - `md5sum`: Stores a md5 checksum of the table and its objects. This can
///   detect even single bitflips, but is computationally expensive.
/// - `sync`: Blocks the process until the file has been successfully written.
///
/// Can have error type `ErlangError`.
pub fn tab2file(
  with oset: OSet(t),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(oset.inner, filename, object_count, md5sum, sync)
}

/// Creates a `OSet` from `filename` that was previously created by `tab2file`.
///
/// For type safety reasons, a dynamic decoder must be provided, and the decoder
/// must not fail for all objects in the table.
///
/// If the flag `verify` is set, then checks are performed to ensure the data is
/// correct. This can be slow if `tab2file` was ran with `md5sum` enabled.
///
/// Size-1 tuples are handled uniquely internally and are treated as non-tuples
/// when saved to disk, so you must decode the non-tuple version of the type.
///
/// Can have error types `DecodeFailure` and `ErlangError`.
pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  using decoder: fn(Dynamic) -> Result(t, _),
) -> Result(OSet(t), BravoError) {
  use res <- result.try(master.file2tab(filename, verify, decoder))
  Ok(OSet(res))
}

/// Returns a list containing all of the objects in the `OSet`.
///
/// The list returned is ordered.
pub fn tab2list(with oset: OSet(t)) -> List(t) {
  master.tab2list(oset.inner)
}

/// Returns and removes an object at `key` in the `OSet`, if such object exists.
pub fn take(with oset: OSet(t), at key: a) -> Result(t, Nil) {
  master.take_set(oset.inner, key)
}

/// Returns whether a `OSet` contains an object at `key`.
pub fn member(with oset: OSet(t), at key: a) -> Bool {
  master.member(oset.inner, key)
}

/// Returns the first key (not the object!) in the table, if it exists.
///
/// `OSet`s _are_ ordered as per the Erlang documentation.
pub fn first(with oset: OSet(t)) -> Result(a, Nil) {
  master.first(oset.inner)
}

/// Returns the last key (not the object!) in the table, if it exists.
///
/// `OSet`s _are_ ordered as per the Erlang documentation.
pub fn last(with oset: OSet(t)) -> Result(a, Nil) {
  master.last(oset.inner)
}

/// Given a key, returns the next key (not the object!) after it in the table,
/// if it exists.
///
/// `OSet`s _are_ ordered as per the Erlang documentation.
pub fn next(with oset: OSet(t), from key: a) -> Result(a, Nil) {
  master.next(oset.inner, key)
}

/// Given a key, returns the previous key (not the object!) before it in the
/// table, if it exists.
///
/// `OSet`s _are_ ordered as per the Erlang documentation.
pub fn prev(with oset: OSet(t), from key: a) -> Result(a, Nil) {
  master.prev(oset.inner, key)
}
