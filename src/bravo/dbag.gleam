//// This module provides functions to work with `DBag`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_option
import gleam/dynamic.{type Dynamic}
import gleam/result

/// A duplicate bag bravo. Keys may occur multiple times per table, and verbatim
/// copies of an object can be stored.
pub opaque type DBag(k, v) {
  DBag(inner: master.InnerTable)
}

pub fn new(
  name name: String,
  access access: Access,
) -> Result(DBag(k, v), BravoError) {
  use res <- result.try(master.new(name, access, new_option.DuplicateBag))
  Ok(DBag(res))
}

pub fn insert(
  into bag: DBag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(bag.inner, key, value)
}

pub fn insert_list(
  dbag: DBag(k, v),
  objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(dbag.inner, objects)
}

pub fn insert_new(
  with dbag: DBag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert_new(dbag.inner, key, value)
}

pub fn lookup(with dbag: DBag(k, v), at key: a) -> Result(List(t), BravoError) {
  master.lookup_bag(dbag.inner, key)
}

pub fn take(with dbag: DBag(k, v), at key: a) -> Result(List(t), BravoError) {
  master.take_bag(dbag.inner, key)
}

pub fn delete(with dbag: DBag(k, v)) -> Result(Nil, BravoError) {
  master.delete(dbag.inner)
}

pub fn delete_key(with dbag: DBag(k, v), at key: a) -> Result(Nil, BravoError) {
  master.delete_key(dbag.inner, key)
}

pub fn delete_all_objects(with dbag: DBag(k, v)) -> Result(Nil, BravoError) {
  master.delete_all_objects(dbag.inner)
}

pub fn delete_object(
  from dbag: DBag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.delete_object(dbag.inner, key, value)
}

pub fn delete_object_tuple(
  from dbag: DBag(k, v),
  object object: #(k, v),
) -> Result(Nil, BravoError) {
  master.delete_object_tuple(dbag.inner, object)
}

pub fn tab2file(
  with dbag: DBag(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(dbag.inner, filename, object_count, md5sum, sync)
}

pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  key_decoder key_decoder: fn(Dynamic) -> Result(k, _),
  value_decoder value_decoder: fn(Dynamic) -> Result(v, _),
) -> Result(DBag(k, v), BravoError) {
  use res <- result.try(master.file2tab(
    filename,
    verify,
    key_decoder,
    value_decoder,
  ))
  Ok(DBag(res))
}

pub fn tab2list(with dbag: DBag(k, v)) -> List(#(k, v)) {
  master.tab2list(dbag.inner)
}

pub fn member(with dbag: DBag(k, v), at key: a) -> Result(Bool, BravoError) {
  master.member(dbag.inner, key)
}

pub fn first(with dbag: DBag(k, v)) -> Result(a, BravoError) {
  master.first(dbag.inner)
}

pub fn last(with dbag: DBag(k, v)) -> Result(a, BravoError) {
  master.last(dbag.inner)
}

pub fn next(with dbag: DBag(k, v), from key: a) -> Result(a, BravoError) {
  master.next(dbag.inner, key)
}

pub fn prev(with dbag: DBag(k, v), from key: a) -> Result(a, BravoError) {
  master.prev(dbag.inner, key)
}
