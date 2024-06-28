//// This module provides functions to work with `USet`s

import bravo.{type Access, type BravoError}
import bravo/bravo_options
import bravo/internal/master
import gleam/dynamic.{type Dynamic}
import gleam/result

/// An unordered set. Keys may only occur once per table,
/// and keys are unordered.
///
/// In order for a match to occur, entries must have the same value _and type_.
pub opaque type USet(k, v) {
  USet(inner: master.InnerTable)
}

/// Creates a new table using default configuration options.
///
/// # Parameters
/// ## `name name: String`
/// The name of the table. All tables must have different names.
/// ## `access access: bravo.Access`
/// Whether the table is `Public`, `Protected`, or `Private`. See docs for
/// `bravo.Access` for more information.
///
/// # Returns
/// ## `Ok(USet(k, v))`
/// A handle to the created table. Tables have static lifetimes, so this handle
/// going out of scope does *not* delete the table.
/// ## `Error(bravo.TableAlreadyExists)`
/// A table with the same `name` already exists. Either delete the existing
/// table or choose a different name.
pub fn new(
  name name: String,
  access access: Access,
) -> Result(USet(k, v), BravoError) {
  use res <- result.try(master.new(name, access, bravo_options.Set))
  Ok(USet(res))
}

/// Creates a new table using a `bravo.Spec`.
///
/// # Parameters
/// ## `spec spec: bravo.Spec`
/// The spec used to create the table. See docs for `bravo.Spec` for more
/// information.
///
/// # Returns
/// ## `Ok(USet(k, v))`
/// A handle to the created table. Tables have static lifetimes, so this handle
/// going out of scope does *not* delete the table.
/// ## `Error(bravo.TableAlreadyExists)`
/// A table with the same `name` already exists. Either delete the existing
/// table or choose a different name
pub fn from_spec(spec spec: bravo.Spec) -> Result(USet(k, v), BravoError) {
  use res <- result.try(master.from_spec(spec, bravo_options.Set))
  Ok(USet(res))
}

/// Inserts a single object into a table. If an object with the same key already
/// exists, it is overwritten.
///
/// # Parameters
/// ## `into uset: USet(k, v)`
/// The table to insert into.
/// ## `key key: k`
/// The key of the inserted object.
/// ## `value value: v`
/// The value of the inserted object.
///
/// # Returns
/// ## `Ok(Nil)`
/// The insertion succeeded.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert(
  into uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(uset.inner, key, value)
}

/// Inserts a list of objects (represented as tuples) into a table. If any
/// objects with the same key already exist, then they are overwritten. If the
/// list contains multiple objects with the same key, only the last one will be
/// saved.
///
/// # Parameters
/// ## `into uset: USet(k, v)`
/// The table to insert into.
/// ## `list objects: List(#(k, v))`
/// The list of objects to insert into the table.
///
/// # Returns
/// ## `Ok(Nil)`
/// The insertion succeeded.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_list(
  into uset: USet(k, v),
  list objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(uset.inner, objects)
}

/// Inserts a single object into a table. Fails if an object with the same key
/// already exists in the table.
///
/// To overwrite on insert, instead use `uset.insert`.
///
/// # Parameters
/// ## `into uset: USet(k, v)`
/// The table to insert into.
/// ## `key key: k`
/// The key of the inserted object.
/// ## `value value: v`
/// The value of the inserted object.
///
/// # Returns
/// ## `Ok(Nil)`
/// The insertion succeeded.
/// ## `Error(bravo.KeyAlreadyPresent)`
/// An object with the same key already existed.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_new(
  into uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert_new(uset.inner, key, value)
}

/// Inserts a list of objects (represented as tuples) into a table. If any
/// objects with the same key already exist, then *none* of the objects in the
/// list are inserted and the function fails.
///
/// To overwrite on insert, instead use `uset.insert_list`.
///
/// # Parameters
/// ## `into uset: USet(k, v)`
/// The table to insert into.
/// ## `list objects: List(#(k, v))`
/// The list of objects to insert into the table.
///
/// # Returns
/// ## `Ok(Nil)`
/// The insertion succeeded.
/// ## `Error(bravo.KeyAlreadyPresent)`
/// An object with the same key already existed.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_new_list(
  into uset: USet(k, v),
  list objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_new_list(uset.inner, objects)
}

/// Looks up an object's value in a table, given that object's key.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to lookup from.
/// ## `at key: k`
/// The key of the object being looked up.
///
/// # Returns
/// ## `Ok(v)`
/// The object exists and here is its value.
/// ## `Error(bravo.Empty)`
/// The object does not exist.
/// ## `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn lookup(from uset: USet(k, v), at key: k) -> Result(v, BravoError) {
  master.lookup_set(uset.inner, key)
}

/// Looks up an object's value in a table, given that object's key. Afterwards,
/// that object is removed from the table.
///
/// To prevent removing the object, instead use `uset.insert`.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to take from.
/// ## `at key: k`
/// The key of the object being taken.
///
/// # Returns
/// ## `Ok(v)`
/// The object exists and here is its value.
/// ## `Error(bravo.Empty)`
/// The object does not exist.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn take(from uset: USet(k, v), at key: k) -> Result(v, BravoError) {
  master.take_set(uset.inner, key)
}

/// Deletes a table. Existing handles to it will become invalid and cause most
/// functions that receive it to return `Error(bravo.TableDoesNotExist)`, even
/// if a new table with the same name is created.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to delete.
///
/// # Returns
/// ## `Ok(Nil)`
/// The table existed and was successfully deleted.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted by a different `delete` call?
pub fn delete(from uset: USet(k, v)) -> Result(Nil, BravoError) {
  master.delete(uset.inner)
}

/// Deletes an object using its key.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to delete from.
/// ## `at key: k`
/// The key of the object to delete.
///
/// # Returns
/// ## `Ok(Nil)`
/// There was an object with the key and it was successfully deleted.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn delete_key(from uset: USet(k, v), at key: k) -> Result(Nil, BravoError) {
  master.delete_key(uset.inner, key)
}

/// Deletes all objects in a table.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to delete from.
///
/// # Returns
/// ## `Ok(Nil)`
/// The objects were deleted successfully. This is returned even if there were
/// no objects to begin with.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn delete_all_objects(from uset: USet(k, v)) -> Result(Nil, BravoError) {
  master.delete_all_objects(uset.inner)
}

/// Deletes an object using both its key and value.
///
/// For `USet` and `OSet`, there is little reason to use this function over
/// `delete_key`.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to delete from.
/// ## `at key: k`
/// The key of the object to delete.
/// ## `value value: v`
/// The value of the object to delete.
///
/// # Returns
/// ## `Ok(Nil)`
/// The table existed and was successfully deleted.
/// ## `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted by a different `delete` call?
@deprecated("For `USet` and `OSet`, there is little reason to use this function over `delete_key`.")
pub fn delete_object(
  from uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.delete_object(uset.inner, key, value)
}

/// Writes a table to disk.
///
/// # Parameters
/// ## `from uset: USet(k, v)`
/// The table to write to disk.
/// ## `to filename: String`
/// The file location (relative to the direcory `gleam run` is executed in) to
/// write the file to. This also doubles as the name of the table when it is
/// turned back into a table.
/// ## `object_count object_count: Bool`
/// The number of objects in the table is written in the file footer. This can
/// be used later to verify the table and make sure there was no truncation.
/// ## `md5sum md5sum: Bool`
/// The table is checksummed using md5, the result of which is written in the
/// file footer. This can detect single bit errors but has a not-insignificant
/// performance cost.
/// ## `sync sync: Bool`
/// Blocks the process until the file is fully written.
///
/// # Returns
/// ## `Ok(Nil)`
/// The table was saved to the file successfully.
/// ## `Error(bravo.NoFilePermissions)`
/// The path is valid, but requires write access that the program does not have.
/// ## `Error(bravo.InvalidPath)`
/// The path is invalid. Do all of the directories in the path exist?
/// ## `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// ## `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn tab2file(
  from uset: USet(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(uset.inner, filename, object_count, md5sum, sync)
}

/// Loads a table from disk.
///
/// # Parameters
/// ## `from filename: String`
/// The name of the file. This also doubles as the name of the table.
/// ## `verify verify: Bool`
/// Verifies the table using the information provided by enabling the
/// `object_count` and/or `md5sum` flags in `tab2file`. If `md5sum` is enabled,
/// this function may be slow. If neither flag is set, then the "size of the
/// table ... when the dump started" is compared to the number of objects
/// written, which may result in issues if the table was written to during the
/// original dump; avoid this by not verifying or setting on of the `tab2file`
/// flags.
/// ## `k key_decoder: fn(Dynamic) -> Result(k, _)`
/// ## `v value_decoder: fn(Dynamic) -> Result(v, _)`
/// Used to ensure at runtime that the type of the table makes sense. The types
/// of the key and value are *not* stored in the file, so you must know what the
/// types are supposed to be when calling this function through other means.
///
/// # Returns
/// ## `Ok(USet(k, v))`
/// The file read succeeded, the table verified successfully, and the types
/// match the decoders. You are given a handle to the new table.
/// ## `Error(bravo.InvalidObjectCount)`
/// `object_count` verification failed. Truncation and/or file tampering may
/// have occured.
/// ## `Error(bravo.ChecksumError)`
/// `md5sum` verification failed. Is the file corrupted?
/// ## `Error(bravo.FileDoesNotExist)`
/// The file specified by filename does not exist.
pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  k key_decoder: fn(Dynamic) -> Result(k, _),
  v value_decoder: fn(Dynamic) -> Result(v, _),
) -> Result(USet(k, v), BravoError) {
  use res <- result.try(master.file2tab(
    filename,
    verify,
    key_decoder,
    value_decoder,
  ))
  Ok(USet(res))
}

pub fn tab2list(with uset: USet(k, v)) -> Result(List(#(k, v)), BravoError) {
  master.tab2list(uset.inner)
}

pub fn member(with uset: USet(k, v), at key: a) -> Result(Bool, BravoError) {
  master.member(uset.inner, key)
}

pub fn first(with uset: USet(k, v)) -> Result(a, BravoError) {
  master.first(uset.inner)
}

pub fn last(with uset: USet(k, v)) -> Result(a, BravoError) {
  master.last(uset.inner)
}

pub fn next(with uset: USet(k, v), from key: a) -> Result(a, BravoError) {
  master.next(uset.inner, key)
}

pub fn prev(with uset: USet(k, v), from key: a) -> Result(a, BravoError) {
  master.prev(uset.inner, key)
}
