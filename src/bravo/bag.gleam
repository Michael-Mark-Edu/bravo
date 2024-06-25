//// This module provides functions to work with `Bag`s

import bravo.{type Access, type BravoError}
import bravo/internal/master
import bravo/internal/new_option
import gleam/dynamic.{type Dynamic}
import gleam/result

/// A bag table. Keys may occur multiple times per table, but objects cannot be
/// copied verbatim.
pub opaque type Bag(k, v) {
  Bag(inner: master.InnerTable)
}

pub fn new(
  name name: String,
  access access: Access,
) -> Result(Bag(k, v), BravoError) {
  use res <- result.try(master.new(name, access, new_option.Bag))
  Ok(Bag(res))
}

pub fn insert(
  into bag: Bag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert(bag.inner, key, value)
}

pub fn insert_list(
  bag: Bag(k, v),
  objects: List(#(k, v)),
) -> Result(Nil, BravoError) {
  master.insert_list(bag.inner, objects)
}

pub fn insert_new(
  into bag: Bag(k, v),
  key key: k,
  value value: v,
) -> Result(Nil, BravoError) {
  master.insert_new(bag.inner, key, value)
}

pub fn lookup(from bag: Bag(k, v), at key: k) -> Result(List(v), BravoError) {
  master.lookup_bag(bag.inner, key)
}

pub fn take(from bag: Bag(k, v), at key: k) -> Result(List(v), BravoError) {
  master.take_bag(bag.inner, key)
}

pub fn delete(bag: Bag(k, v)) -> Result(Nil, BravoError) {
  master.delete(bag.inner)
}

pub fn delete_key(from bag: Bag(k, v), at key: k) -> Nil {
  master.delete_key(bag.inner, key)
}

pub fn delete_all_objects(bag: Bag(k, v)) -> Nil {
  master.delete_all_objects(bag.inner)
}

pub fn delete_object(from bag: Bag(k, v), object object: #(k, v)) -> Nil {
  master.delete_object(bag.inner, object)
}

pub fn tab2file(
  from bag: Bag(k, v),
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  master.tab2file(bag.inner, filename, object_count, md5sum, sync)
}

pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  key_decoder key_decoder: fn(Dynamic) -> Result(k, _),
  value_decoder value_decoder: fn(Dynamic) -> Result(v, _),
) -> Result(Bag(k, v), BravoError) {
  use res <- result.try(master.file2tab(
    filename,
    verify,
    key_decoder,
    value_decoder,
  ))
  Ok(Bag(res))
}

pub fn tab2list(bag: Bag(k, v)) -> List(#(k, v)) {
  master.tab2list(bag.inner)
}

pub fn member(of bag: Bag(k, v), at key: a) -> Bool {
  master.member(bag.inner, key)
}

pub fn first(bag: Bag(k, v)) -> Result(a, Nil) {
  master.first(bag.inner)
}

pub fn last(bag: Bag(k, v)) -> Result(a, Nil) {
  master.last(bag.inner)
}

pub fn next(with bag: Bag(k, v), from key: a) -> Result(a, Nil) {
  master.next(bag.inner, key)
}

pub fn prev(with bag: Bag(k, v), from key: a) -> Result(a, Nil) {
  master.prev(bag.inner, key)
}
