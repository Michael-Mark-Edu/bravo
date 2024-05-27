import bravo/error.{type ErlangError}
import bravo/internal/bindings
import bravo/internal/new_option
import bravo/object.{type Object}
import bravo/table.{type Access, type Set}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn new(
  name: Atom,
  size: Int,
  access: Access,
  keypos: Int,
  stringed: Bool,
) -> Result(Set, Option(ErlangError)) {
  case keypos > size || size < 2 || size > 9 || keypos < 1 || keypos > 9 {
    True -> Error(None)
    False -> {
      case
        bindings.new(name, [
          new_option.Set,
          case access {
            table.Public -> new_option.Public
            table.Protected -> new_option.Protected
            table.Private -> new_option.Private
          },
          new_option.NamedTable,
          new_option.Keypos(keypos),
        ])
      {
        Error(e) -> Error(Some(e))
        Ok(a) -> Ok(table.Set(a, size, keypos, stringed))
      }
    }
  }
}

pub fn insert(set: Set, objects: List(Object(a))) -> Bool {
  let testlist =
    list.map(objects, fn(obj) {
      case set.stringed, set.size, obj {
        False, 2, object.O2(..) -> True
        False, 3, object.O3(..) -> True
        False, 4, object.O4(..) -> True
        False, 5, object.O5(..) -> True
        False, 6, object.O6(..) -> True
        False, 7, object.O7(..) -> True
        False, 8, object.O8(..) -> True
        False, 9, object.O9(..) -> True
        False, _, _ -> False
        True, 2, object.S2(..) -> True
        True, 3, object.S3(..) -> True
        True, 4, object.S4(..) -> True
        True, 5, object.S5(..) -> True
        True, 6, object.S6(..) -> True
        True, 7, object.S7(..) -> True
        True, 8, object.S8(..) -> True
        True, 9, object.S9(..) -> True
        True, _, _ -> False
      }
    })
  let testlist = list.filter(testlist, fn(b) { !b })
  case list.length(testlist) {
    0 -> bindings.set_insert(set, objects)
    _ -> False
  }
}

pub fn lookup(set: Set, key: a) -> Option(Object(b)) {
  bindings.set_lookup(set, key)
}
