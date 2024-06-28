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

pub fn new(
  name name: String,
  access access: Access,
) -> Result(USet(k, v), BravoError) {
  use res <- result.try(master.new(name, access, bravo_options.Set))
  Ok(USet(res))
}

pub fn from_spec(spec spec: bravo.Spec) -> Result(USet(k, v), BravoError) {
  use res <- result.try(master.from_spec(spec, bravo_options.Set))
  Ok(USet(res))
}

pub fn insert(
  with uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(uset.inner, key, value)
}

pub fn insert_list(
  uset: USet(k, v),
  objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(uset.inner, objects)
}

pub fn insert_new(
  with uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert_new(uset.inner, key, value)
}

pub fn insert_new_list(
  uset: USet(k, v),
  objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_new_list(uset.inner, objects)
}

pub fn lookup(with uset: USet(k, v), at key: k) -> Result(t, BravoError) {
  master.lookup_set(uset.inner, key)
}

pub fn take(with uset: USet(k, v), at key: k) -> Result(t, BravoError) {
  master.take_set(uset.inner, key)
}

pub fn delete(with uset: USet(k, v)) -> Result(Nil, BravoError) {
  master.delete(uset.inner)
}

pub fn delete_key(with uset: USet(k, v), at key: a) -> Result(Nil, BravoError) {
  master.delete_key(uset.inner, key)
}

pub fn delete_all_objects(with uset: USet(k, v)) -> Result(Nil, BravoError) {
  master.delete_all_objects(uset.inner)
}

pub fn delete_object(
  from uset: USet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.delete_object(uset.inner, key, value)
}

pub fn delete_object_tuple(
  from uset: USet(k, v),
  object object: #(k, v),
) -> Result(Nil, BravoError) {
  master.delete_object_tuple(uset.inner, object)
}

pub fn tab2file(
  with uset: USet(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(uset.inner, filename, object_count, md5sum, sync)
}

pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  key_decoder key_decoder: fn(Dynamic) -> Result(k, _),
  value_decoder value_decoder: fn(Dynamic) -> Result(v, _),
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
