import bravo/error.{type ErlangError}
import bravo/internal/bindings
import bravo/internal/new_option
import bravo/object.{type Object}
import bravo/table.{type Access, type USet}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn new(
  name: Atom,
  size: Int,
  access: Access,
  keypos: Int,
) -> Result(USet, Option(ErlangError)) {
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
        Ok(a) -> Ok(table.USet(a, size, keypos))
      }
    }
  }
}

pub fn insert(uset: USet, objects: List(a)) -> Bool {
  bindings.uset_insert(uset, objects)
}

pub fn insert_obj(uset: USet, objects: List(Object(a))) -> Bool {
  list.map(objects, object.extract)
  |> insert(uset, _)
}

pub fn lookup(uset: USet, key: a) -> Option(Object(b)) {
  case bindings.try_lookup(uset, key) {
    [res] -> Some(object.new(res))
    _ -> None
  }
}
