//// This module provides functions to work with `Bag`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_option
import gleam/dynamic.{type Dynamic}
import gleam/result

/// A bag table. Keys may occur multiple times per table, but objects cannot be
/// copied verbatim.
pub opaque type Bag(t) {
  Bag(inner: master.InnerTable)
}

/// Creates a new ETS table configured as a bag: keys may only occur multiple
/// times per table, but objects cannot be copied verbatim.
///
/// `name`: An atom representing the name of the `Bag`. There may only be one
/// ETS table associated with an atom.
/// `keypos`: The index (1-indexed) that represents the key position of the
/// object. This function fails if this is less than 1.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `Bag`.
/// - `Protected`: Any process can read the `Bag`. Only the owner process can
///   write to it.
/// - `Private`: Only the parent process can read or write to the `Bag`.
///
/// Returns a result of the created `Bag`, which can be used by other functions
/// in this module.
///
/// Can have error types:
/// - `Error(TableAlreadyExists)`: A table with the same name already exists.
///   Deleting this other table will free up the spot.
/// - `Error(NonPositiveKeypos)`: The input `keypos` is 0 or less.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
pub fn new(
  name name: String,
  keypos keypos: Int,
  access access: Access,
) -> Result(Bag(t), BravoError) {
  use res <- result.try(master.new(name, keypos, access, new_option.Bag))
  Ok(Bag(res))
}

/// Inserts a list of tuples into a `Bag`.
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
/// If an object with the same key already exists, the two objects will be added
/// and coexist. If they are identical objects, then nothing will be inserted.
///
/// To instead get an error when inserting an object with the same key as an
/// already existing object, use `insert_new`.
pub fn insert(
  with bag: Bag(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert(bag.inner, objects)
}

/// Inserts a list of tuples into a `Bag`. Unlike `insert`, this cannot
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
  with bag: Bag(t),
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  master.insert_new(bag.inner, objects)
}

/// Gets a list of objects from a `Bag`.
///
/// Returns a list containing the objects, if any match. Otherwise:
/// - `Error(Empty)`: No objects have the given key. This is returned instead of
///   an `Ok([])`.
/// - `Error(UninitializedTable)`: The table has never been `insert`ed into and
///   was not created using `file2tab`.
/// - `Error(TableDoesNotExist)`: The table was either deleted or never created.
/// - `Error(AccessDenied)`: The table has an access level of `Private` and is
///   owned by a different process.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
pub fn lookup(with bag: Bag(t), at key: a) -> Result(List(t), BravoError) {
  master.lookup_bag(bag.inner, key)
}

/// Returns and removes all objects with `key` in the `Bag`.
///
/// Returns a list containing the objects, if any match. Otherwise:
/// - `Error(Empty)`: No objects have the given key. This is returned instead of
///   an `Ok([])`.
/// - `Error(UninitializedTable)`: The table has never been `insert`ed into and
///   was not created using `file2tab`.
/// - `Error(TableDoesNotExist)`: The table was either deleted or never created.
/// - `Error(AccessDenied)`: The table has an access level of `Private` and is
///   owned by a different process.
/// - `Error(ErlangError)`: Likely a bug with the library itself. Please report.
pub fn take(with bag: Bag(t), at key: a) -> Result(List(t), BravoError) {
  master.take_bag(bag.inner, key)
}

/// Deletes a `Bag`.
///
/// Table lifetime is static, and memory is only freed when the owner process is
/// killed! Don't forget to call this function if you intend for this table to
/// be temporary!
///
/// The input `Bag` is completely useless after it is deleted. Even if another
/// table is created with the same name, the old handle will not work.
pub fn delete(with bag: Bag(t)) -> Bool {
  master.delete(bag.inner)
}

/// Deletes all objects addressed by `key`, if any exist. If nothing does, this
/// does nothing.
pub fn delete_key(with bag: Bag(t), at key: a) -> Nil {
  master.delete_key(bag.inner, key)
}

/// Deletes all objects in the `Bag`. This is atomic and isolated.
pub fn delete_all_objects(with bag: Bag(t)) -> Nil {
  master.delete_all_objects(bag.inner)
}

/// Deletes a specific object in the `Bag`. Other objects with the same key are
/// unaffected.
pub fn delete_object(with bag: Bag(t), target object: t) -> Nil {
  master.delete_object(bag.inner, object)
}

/// Saves a `Bag` as file `filename` that can later be read back into memory
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
  with bag: Bag(t),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(bag.inner, filename, object_count, md5sum, sync)
}

/// Creates a `Bag` from `filename` that was previously created by `tab2file`.
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
) -> Result(Bag(t), BravoError) {
  use res <- result.try(master.file2tab(filename, verify, decoder))
  Ok(Bag(res))
}

/// Returns a list containing all of the objects in the `Bag`.
pub fn tab2list(with bag: Bag(t)) -> List(t) {
  master.tab2list(bag.inner)
}

/// Returns whether a `Bag` contains an object at `key`.
pub fn member(with bag: Bag(t), at key: a) -> Bool {
  master.member(bag.inner, key)
}

/// Returns the first key (not the object!) in the table, if it exists.
///
/// `Bag`s are unordered, so the order of keys is unknown.
pub fn first(with bag: Bag(t)) -> Result(a, Nil) {
  master.first(bag.inner)
}

/// Returns the last key (not the object!) in the table, if it exists.
///
/// `Bag`s are unordered, so the order of keys is unknown.
pub fn last(with bag: Bag(t)) -> Result(a, Nil) {
  master.last(bag.inner)
}

/// Given a key, returns the next key (not the object!) after it in the table,
/// if it exists.
///
/// `Bag`s are unordered, so the order of keys is unknown.
pub fn next(with bag: Bag(t), from key: a) -> Result(a, Nil) {
  master.next(bag.inner, key)
}

/// Given a key, returns the previous key (not the object!) before it in the
/// table, if it exists.
///
/// `Bag`s are unordered, so the order of keys is unknown.
pub fn prev(with bag: Bag(t), from key: a) -> Result(a, Nil) {
  master.prev(bag.inner, key)
}
