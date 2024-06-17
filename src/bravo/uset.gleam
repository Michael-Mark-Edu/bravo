//// This module provides functions to work with `USet`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_option
import gleam/dynamic.{type Dynamic}
import gleam/result

/// An unordered set. Keys may only occur once per table,
/// and keys are unordered.
///
/// In order for a match to occur, entries must have the same value _and type_.
pub opaque type USet(t) {
  USet(inner: master.InnerTable)
}

/// Creates a new ETS table configured as a set: keys may only occur once per
/// table, and objects are unordered.
///
/// `name`: An atom representing the name of the `USet`. There may only be one
/// ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the
/// object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `USet`.
/// - `Protected`: Any process can read the `USet`. Only the owner process can
///   write to it.
/// - `Private`: Only the parent process can read or write to the `USet`.
///
/// Returns a result of the created `USet`, which can be used by other functions
/// in this module.
///
/// Can have error types `ErlangError` and `NonPositiveKeypos`.
pub fn new(
  name name: String,
  keypos keypos: Int,
  access access: Access,
) -> Result(USet(t), BravoError) {
  use res <- result.try(master.new(name, keypos, access, new_option.Set))
  Ok(USet(res))
}

/// Inserts a list of tuples into a `USet`.
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
  with uset: USet(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert(uset.inner, objects)
}

/// Inserts a list of tuples into a `USet`. Unlike `insert`, this cannot
/// overwrite objects and will error if it tries to do so.
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
  with uset: USet(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert_new(uset.inner, objects)
}

/// Gets an object from a `USet`.
///
/// Returns an `Result` containing the object, if it exists. Otherwise:
/// - `Error(Empty)`: There is no object with the given key.
/// - `Error(UninitializedTable)`: The table has never been `insert`ed into and
///   was not created using `file2tab`.
/// - `Error(TableDoesNotExist)`: The table was either deleted or never created.
/// - `Error(AccessDenied)`: The table has an access level of `Private` and is
///   owned by a different process.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
pub fn lookup(with uset: USet(t), at key: a) -> Result(t, BravoError) {
  master.lookup_set(uset.inner, key)
}

/// Deletes a `USet`.
///
/// Table lifetime is static, and memory is only freed when the owner process is
/// killed! Don't forget to call this function if you intend for this table to
/// be temporary!
///
/// The input `USet` is completely useless after it is deleted. Even if another
/// table is created with the same name, the old handle will not work.
pub fn delete(with uset: USet(t)) -> Bool {
  master.delete(uset.inner)
}

/// Deletes the object addressed by `key`, if it exists. If it doesn't, this
/// does nothing.
pub fn delete_key(with uset: USet(t), at key: a) -> Nil {
  master.delete_key(uset.inner, key)
}

/// Deletes all objects in the `USet`. This is atomic and isolated.
pub fn delete_all_objects(with uset: USet(t)) -> Nil {
  master.delete_all_objects(uset.inner)
}

/// Deletes a specific object in the `USet`. This is more useful with
/// `Bag`s and `DBag`s.
pub fn delete_object(with uset: USet(t), target object: t) -> Nil {
  master.delete_object(uset.inner, object)
}

/// Saves a `USet` as file `filename` that can later be read back into memory
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
  with uset: USet(t),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(uset.inner, filename, object_count, md5sum, sync)
}

/// Creates a `USet` from `filename` that was previously created by `tab2file`.
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
) -> Result(USet(t), BravoError) {
  use res <- result.try(master.file2tab(filename, verify, decoder))
  Ok(USet(res))
}

/// Returns a list containing all of the objects in the `USet`.
pub fn tab2list(with uset: USet(t)) -> List(t) {
  master.tab2list(uset.inner)
}

/// Returns and removes an object at `key` in the `USet`, if such object exists.
pub fn take(with uset: USet(t), at key: a) -> Result(t, Nil) {
  master.take_set(uset.inner, key)
}

/// Returns whether a `USet` contains an object at `key`.
pub fn member(with uset: USet(t), at key: a) -> Bool {
  master.member(uset.inner, key)
}

/// Returns the first key (not the object!) in the table, if it exists.
///
/// `USet`s are unordered, so the order of keys is unknown.
pub fn first(with uset: USet(t)) -> Result(a, Nil) {
  master.first(uset.inner)
}

/// Returns the last key (not the object!) in the table, if it exists.
///
/// `USet`s are unordered, so the order of keys is unknown.
pub fn last(with uset: USet(t)) -> Result(a, Nil) {
  master.last(uset.inner)
}

/// Given a key, returns the next key (not the object!) after it in the table,
/// if it exists.
///
/// `USet`s are unordered, so the order of keys is unknown.
pub fn next(with uset: USet(t), from key: a) -> Result(a, Nil) {
  master.next(uset.inner, key)
}

/// Given a key, returns the previous key (not the object!) before it in the
/// table, if it exists.
///
/// `USet`s are unordered, so the order of keys is unknown.
pub fn prev(with uset: USet(t), from key: a) -> Result(a, Nil) {
  master.prev(uset.inner, key)
}
