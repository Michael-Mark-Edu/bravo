//// This module provides the `Object` type and functions that use it. `Object`s are essential to accessing any ETS table.

import bravo/internal/bindings
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}

/// Represents an object in an ETS table. Functionally, this is simply a wrapper around arbitrary tuples.
///
pub opaque type Object(a) {
  Object(raw: a)
}

/// Creates a new `Object` from a tuple. Passing in a non-tuple may result in unexpected behavior.
///
pub fn new(tuple: a) -> Object(a) {
  Object(tuple)
}

/// Creates a `Dynamic` from the data stored inside an `Object`. This should always be a tuple.
///
pub fn extract(object: Object(a)) -> Dynamic {
  dynamic.from(object.raw)
}

/// Gets the nth element of an `Object` (currently 1-indexed but this may change). Fails if the index is out of range.
///
pub fn element(object: Object(a), index: Int) -> Option(Dynamic) {
  case index <= size(object) || index >= 1 {
    True -> Some(dynamic.from(bindings.element(index, object)))
    False -> None
  }
}

/// Get the number of elements in an `Object`.
///
pub fn size(object: Object(a)) -> Int {
  bindings.tuple_size(object.raw)
}
