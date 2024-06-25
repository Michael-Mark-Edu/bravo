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
pub opaque type OSet(k, v) {
  OSet(inner: master.InnerTable)
}

pub fn new(
  name name: String,
  access access: Access,
) -> Result(OSet(k, v), BravoError) {
  use res <- result.try(master.new(name, access, new_option.OrderedSet))
  Ok(OSet(res))
}

pub fn insert(
  with oset: OSet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(oset.inner, key, value)
}

pub fn insert_list(
  oset: OSet(k, v),
  objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(oset.inner, objects)
}

pub fn insert_new(
  with oset: OSet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(oset.inner, key, value)
}

pub fn lookup(with oset: OSet(k, v), at key: a) -> Result(t, BravoError) {
  master.lookup_set(oset.inner, key)
}

pub fn take(with oset: OSet(k, v), at key: a) -> Result(t, BravoError) {
  master.take_set(oset.inner, key)
}

pub fn delete(with oset: OSet(k, v)) -> Result(Nil, BravoError) {
  master.delete(oset.inner)
}

pub fn delete_key(with oset: OSet(k, v), at key: a) -> Result(Nil, BravoError) {
  master.delete_key(oset.inner, key)
}

pub fn delete_all_objects(with oset: OSet(k, v)) -> Result(Nil, BravoError) {
  master.delete_all_objects(oset.inner)
}

pub fn delete_object(
  from oset: OSet(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.delete_object(oset.inner, key, value)
}

pub fn delete_object_tuple(
  from oset: OSet(k, v),
  object object: #(k, v),
) -> Result(Nil, BravoError) {
  master.delete_object_tuple(oset.inner, object)
}

pub fn tab2file(
  with oset: OSet(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(oset.inner, filename, object_count, md5sum, sync)
}

pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  key_decoder key_decoder: fn(Dynamic) -> Result(k, _),
  value_decoder value_decoder: fn(Dynamic) -> Result(v, _),
) -> Result(OSet(k, v), BravoError) {
  use res <- result.try(master.file2tab(
    filename,
    verify,
    key_decoder,
    value_decoder,
  ))
  Ok(OSet(res))
}

pub fn tab2list(with oset: OSet(k, v)) -> List(#(k, v)) {
  master.tab2list(oset.inner)
}

pub fn member(with oset: OSet(k, v), at key: a) -> Result(Bool, BravoError) {
  master.member(oset.inner, key)
}

pub fn first(with oset: OSet(k, v)) -> Result(a, Nil) {
  master.first(oset.inner)
}

pub fn last(with oset: OSet(k, v)) -> Result(a, Nil) {
  master.last(oset.inner)
}

pub fn next(with oset: OSet(k, v), from key: a) -> Result(a, Nil) {
  master.next(oset.inner, key)
}

pub fn prev(with oset: OSet(k, v), from key: a) -> Result(a, Nil) {
  master.prev(oset.inner, key)
}
