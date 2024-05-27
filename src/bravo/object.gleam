import bravo/internal/bindings
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}

pub opaque type Object(a) {
  Object(raw: a)
}

pub fn new(tuple: a) -> Object(a) {
  Object(tuple)
}

pub fn extract(object: Object(a)) -> Dynamic {
  dynamic.from(object.raw)
}

pub fn element(object: Object(a), index: Int) -> Option(Dynamic) {
  case index <= size(object) || index >= 1 {
    True -> Some(dynamic.from(bindings.element(index, object)))
    False -> None
  }
}

pub fn size(object: Object(a)) -> Int {
  bindings.tuple_size(object.raw)
}
