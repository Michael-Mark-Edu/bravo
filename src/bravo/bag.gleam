//// This module provides functions to work with `Bag`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_options
import gleam/dynamic.{type Dynamic}
import gleam/result

/// A bag table. Keys may occur multiple times per table, but exact key-value
/// pairs are automatically deduped.
pub opaque type Bag(k, v) {
  Bag(inner: master.InnerTable)
}

/// Creates a new table using Erlang's default configuration options. These
/// defaults are generally optimized for synchronous performance, as all async
/// settings are disabled.
///
/// If maximizing performance is desired, instead use `from_spec` instead, as it
/// allows for fine-tuning performance settings to specific use cases.
///
/// It should be noted that the type of the table is not defined at all by this
/// function. This is intentional, as it allows a call to `insert` et al. to
/// coerce the type automatically, so the user shouldn't have to think about
/// type safety too much.
///
/// # Parameters
/// - `name name: String`
/// The name of the table. All tables must have different names.
/// - `access access: bravo.Access`
/// Whether the table is `Public`, `Protected`, or `Private`. See docs for
/// `bravo.Access` for more information.
///
/// # Returns
/// - `Ok(Bag(k, v))`
/// A handle to the created table. Tables have static lifetimes, so this handle
/// going out of scope does *not* delete the table.
/// - `Error(bravo.TableAlreadyExists)`
/// A table with the same `name` already exists. Either delete the existing
/// table or choose a different name.
pub fn new(
  name name: String,
  access access: Access,
) -> Result(Bag(k, v), BravoError) {
  use res <- result.try(master.new(name, access, new_options.Bag))
  Ok(Bag(res))
}

/// Creates a new table using a `bravo.Spec`.
///
/// # Parameters
/// - `spec spec: bravo.Spec`
/// The spec used to create the table. See docs for `bravo.Spec` for more
/// information.
///
/// # Returns
/// - `Ok(Bag(k, v))`
/// A handle to the created table. Tables have static lifetimes, so this handle
/// going out of scope does *not* delete the table.
/// - `Error(bravo.TableAlreadyExists)`
/// A table with the same `name` already exists. Either delete the existing
/// table or choose a different name
pub fn from_spec(spec spec: bravo.Spec) -> Result(Bag(k, v), BravoError) {
  use res <- result.try(master.from_spec(spec, new_options.Bag))
  Ok(Bag(res))
}

/// Inserts a single object into a table. If an object with the same key already
/// exists, then both will coexist. If an object with the same key and value
/// already exists, one will be deleted.
///
/// # Parameters
/// - `into bag: B(k, v)`
/// The table to insert into.
/// - `key key: k`
/// The key of the inserted object.
/// - `value value: v`
/// The value of the inserted object.
///
/// # Returns
/// - `Ok(Nil)`
/// The insertion succeeded.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert(
  into bag: Bag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(bag.inner, key, value)
}

/// Inserts a list of objects (represented as tuples) into a table. If any
/// objects with the same key already exist, then they will all coexist. The
/// same happens if the inserted list contains multiple objects with the same
/// key. Objects that share the same key and value as another will be deleted so
/// that there is only one.
///
/// # Parameters
/// - `into bag: Bag(k, v)`
/// The table to insert into.
/// - `list objects: List(#(k, v))`
/// The list of objects to insert into the table.
///
/// # Returns
/// - `Ok(Nil)`
/// The insertion succeeded.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_list(
  into bag: Bag(k, v),
  list objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(bag.inner, objects)
}

/// Inserts a single object into a table. Fails if an object with the same key
/// already exists in the table.
///
/// To have multiple objects with the same key in a table, instead use
/// `bag.insert`.
///
/// # Parameters
/// - `into bag: Bag(k, v)`
/// The table to insert into.
/// - `key key: k`
/// The key of the inserted object.
/// - `value value: v`
/// The value of the inserted object.
///
/// # Returns
/// - `Ok(Nil)`
/// The insertion succeeded.
/// - `Error(bravo.KeyAlreadyPresent)`
/// An object with the same key already existed.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_new(
  into bag: Bag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert_new(bag.inner, key, value)
}

/// Inserts a list of objects (represented as tuples) into a table. If any
/// objects with the same key already exist, then *none* of the objects in the
/// list are inserted and the function fails.
///
/// To have multiple objects with the same key in a table, instead use
/// `bag.insert_list`.
///
/// # Parameters
/// - `into bag: Bag(k, v)`
/// The table to insert into.
/// - `list objects: List(#(k, v))`
/// The list of objects to insert into the table.
///
/// # Returns
/// - `Ok(Nil)`
/// The insertion succeeded.
/// - `Error(bravo.KeyAlreadyPresent)`
/// An object with the same key already existed.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn insert_new_list(
  into bag: Bag(k, v),
  list objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_new_list(bag.inner, objects)
}

/// Looks up a key and returns a list of objects with that key.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to lookup from.
/// - `at key: k`
/// The key of the object(s) being looked up.
///
/// # Returns
/// - `Ok(v)`
/// There was at least one match and here is its value.
/// - `Error(bravo.Empty)`
/// There are no objects with the given key. Notably, this is used instead of
/// `Ok([])` for parity with the set-type tables which error when no objects are
/// looked up.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn lookup(from bag: Bag(k, v), at key: k) -> Result(List(v), BravoError) {
  master.lookup_bag(bag.inner, key)
}

/// Looks up a key and returns a list of objects with that key. The looked up
/// objects are then removed from the table.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to lookup from.
/// - `at key: k`
/// The key of the object(s) being looked up.
///
/// # Returns
/// - `Ok(v)`
/// There was at least one match and here is its value.
/// - `Error(bravo.Empty)`
/// There are no objects with the given key. Notably, this is used instead of
/// `Ok([])` for parity with the set-type tables which error when no objects are
/// looked up.
/// - `Error(bravo.AccessDenied)`
/// The table is protected private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn take(from bag: Bag(k, v), at key: k) -> Result(List(v), BravoError) {
  master.take_bag(bag.inner, key)
}

/// Deletes a table. Existing handles to it will become invalid and cause most
/// functions that receive it to return `Error(bravo.TableDoesNotExist)`, even
/// if a new table with the same name is created.
///
/// # Parameters
/// - `table bag: Bag(k, v)`
/// The table to delete.
///
/// # Returns
/// - `Ok(Nil)`
/// The table existed and was successfully deleted.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted by a different `delete` call?
pub fn delete(table bag: Bag(k, v)) -> Result(Nil, BravoError) {
  master.delete(bag.inner)
}

/// Deletes all objects with the same key.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to delete from.
/// - `at key: k`
/// The key of the object to delete.
///
/// # Returns
/// - `Ok(Nil)`
/// There was an object with the key and it was successfully deleted.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn delete_key(from bag: Bag(k, v), at key: k) -> Result(Nil, BravoError) {
  master.delete_key(bag.inner, key)
}

/// Deletes all objects in a table.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to delete from.
///
/// # Returns
/// - `Ok(Nil)`
/// The objects were deleted successfully. This is returned even if there were
/// no objects to begin with.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn delete_all_objects(bag: Bag(k, v)) -> Result(Nil, BravoError) {
  master.delete_all_objects(bag.inner)
}

/// Deletes an object using both its key and value. Has no effect on other
/// objects with the same key.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to delete from.
/// - `key key: k`
/// The key of the object to delete.
/// - `value value: v`
/// The value of the object to delete.
///
/// # Returns
/// - `Ok(Nil)`
/// The table existed and was successfully deleted.
/// - `Error(bravo.AccessDenied)`
/// The table is protected or private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted by a different `delete` call?
pub fn delete_object(
  from bag: Bag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.delete_object(bag.inner, key, value)
}

/// Writes a table to disk.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to write to disk.
/// - `to filename: String`
/// The file location (relative to the direcory `gleam run` is executed in) to
/// write the file to. This also doubles as the name of the table when it is
/// turned back into a table.
/// - `object_count object_count: Bool`
/// The number of objects in the table is written in the file footer. This can
/// be used later to verify the table and make sure there was no truncation.
/// - `md5sum md5sum: Bool`
/// The table is checksummed using md5, the result of which is written in the
/// file footer. This can detect single bit errors but has a not-insignificant
/// performance cost.
/// - `sync sync: Bool`
/// Blocks the process until the file is fully written.
///
/// # Returns
/// - `Ok(Nil)`
/// The table was saved to the file successfully.
/// - `Error(bravo.NoFilePermissions)`
/// The path is valid, but requires write access that the program does not have.
/// - `Error(bravo.InvalidPath)`
/// The path is invalid. Do all of the directories in the path exist?
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn tab2file(
  from bag: Bag(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(bag.inner, filename, object_count, md5sum, sync)
}

/// Loads a table from disk.
///
/// # Parameters
/// - `from filename: String`
/// The name of the file. This also doubles as the name of the table.
/// - `verify verify: Bool`
/// Verifies the table using the information provided by enabling the
/// `object_count` and/or `md5sum` flags in `tab2file`. If `md5sum` is enabled,
/// this function may be slow. If neither flag is set, then the "size of the
/// table ... when the dump started" is compared to the number of objects
/// written, which may result in issues if the table was written to during the
/// original dump; avoid this by setting `verify` to false, or setting either
/// `tab2file`'s `object_count` or `md5sum` flags to `true`.
/// - `k key_decoder: fn(Dynamic) -> Result(k, _),
///   v value_decoder: fn(Dynamic) -> Result(v, _)`
/// Used to ensure at runtime that the type of the table makes sense. The types
/// of the key and value are *not* stored in the file, so you must know what the
/// types are supposed to be when calling this function through other means.
///
/// # Returns
/// - `Ok(Bag(k, v))`
/// The file read succeeded, the table verified successfully, and the types
/// match the decoders. You are given a handle to the new table.
/// - `Error(bravo.InvalidObjectCount)`
/// `object_count` verification failed. Truncation and/or file tampering may
/// have occured.
/// - `Error(bravo.ChecksumError)`
/// `md5sum` verification failed. Is the file corrupted?
/// - `Error(bravo.FileDoesNotExist)`
/// The file specified by filename does not exist.
pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  k key_decoder: fn(Dynamic) -> Result(k, _),
  v value_decoder: fn(Dynamic) -> Result(v, _),
) -> Result(Bag(k, v), BravoError) {
  use res <- result.try(master.file2tab(
    filename,
    verify,
    key_decoder,
    value_decoder,
  ))
  Ok(Bag(res))
}

/// Gets a list of all objects in a table.
///
/// # Parameters
/// - `from bag: Bag(k, v)`
/// The table to get a list from.
///
/// # Returns
/// - `Ok(List(#(k, v)))`
/// The table's contents were read succesfully.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn tab2list(bag: Bag(k, v)) -> Result(List(#(k, v)), BravoError) {
  master.tab2list(bag.inner)
}

/// Returns whether a table contains any objects with a key. Similar to `lookup`
/// except that it doesn't return any objects.
///
/// # Parameters
/// - `of bag: Bag(k, v)`
/// The table to test for.
/// - `at key: k`
/// The key of the object being tested.
///
/// # Returns
/// - `Ok(Bool)
/// Whether or not the table contains any objects with the given key.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn member(of bag: Bag(k, v), at key: k) -> Result(Bool, BravoError) {
  master.member(bag.inner, key)
}

/// Gets the key of the first object in a table.
///
/// `Bag`s are unordered, so there is no guarantee of order in the result.
///
/// This function gets keys, not objects, so if a key has multiple objects
/// assigned to it it still only appears once in the first/last/next/prev chain.
///
/// # Parameters
/// - `of bag: Bag(k, v)`
/// The table to get the first object of.
///
/// # Returns
/// - `Ok(k)`
/// There is at least one object in the table and the objects were read
/// successfully.
/// - `Error(bravo.EndOfTable)`
/// The table is empty.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn first(of bag: Bag(k, v)) -> Result(k, BravoError) {
  master.first(bag.inner)
}

/// Gets the key of the last object in a table.
///
/// `Bag`s are unordered, so there is no guarantee of order in the result.
///
/// This function gets keys, not objects, so if a key has multiple objects
/// assigned to it it still only appears once in the first/last/next/prev chain.
///
/// # Parameters
/// - `of bag: Bag(k, v)`
/// The table to get the last object of.
///
/// # Returns
/// - `Ok(k)`
/// There is at least one object in the table and the objects were read
/// successfully.
/// - `Error(bravo.EndOfTable)`
/// The table is empty.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn last(of bag: Bag(k, v)) -> Result(k, BravoError) {
  master.last(bag.inner)
}

/// Given a key, get the next key after it in a table.
///
/// `Bag`s are unordered, so there is no guarantee of order in the result.
///
/// This function gets keys, not objects, so if a key has multiple objects
/// assigned to it it still only appears once in the first/last/next/prev chain.
///
/// # Parameters
/// - `in bag: Bag(k, v)`
/// The table to get the next object in.
/// - `from key: k`
/// The key to look after.
///
/// # Returns
/// - `Ok(k)`
/// The given key is not the last key of the table and the objects were read
/// successfully.
/// - `Error(bravo.EndOfTable)`
/// The given key is the last key of the table, or the table is empty.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn next(in bag: Bag(k, v), from key: k) -> Result(k, BravoError) {
  master.next(bag.inner, key)
}

/// Given a key, get the previous key before it in a table.
///
/// `Bag`s are unordered, so there is no guarantee of order in the result.
///
/// This function gets keys, not objects, so if a key has multiple objects
/// assigned to it it still only appears once in the first/last/next/prev chain.
///
/// # Parameters
/// - `in bag: Bag(k, v)`
/// The table to get the previous key in.
/// - `from key: k`
/// The key to look before.
///
/// # Returns
/// - `Ok(k)`
/// The given key is not the first key of the table and the objects were read
/// successfully.
/// - `Error(bravo.EndOfTable)`
/// The given key is the first key of the table, or the table is empty.
/// - `Error(bravo.AccessDenied)`
/// The table is private and the current process does not own it.
/// - `Error(bravo.TableDoesNotExist)`
/// The table given does not exist. Was it deleted?
pub fn prev(in bag: Bag(k, v), from key: k) -> Result(k, BravoError) {
  master.prev(bag.inner, key)
}
