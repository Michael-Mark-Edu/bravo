import bravo/error.{type ErlangError}
import bravo/internal/bindings
import bravo/internal/new_option
import bravo/object.{type Object}
import bravo/table.{type Access, type USet}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}

/// Creates a new ETS table configured as a set: keys may only occur once per table, and objects are unordered.
///
/// `name`: An atom representing the name of the `USet`. There may only be one ETS table associated with an atom.
/// `size`: The _minimum_ size of an object in the `USet`. Attempting to insert an object less than this will fail. Must be at least 2.
/// `access`: Determines how visible the table is to other processes.
/// - `Public`: Any process can read or write to the `USet`.
/// - `Protected`: Any process can read the `USet`. Only the owner process can write to it.
/// - `Private`: Only the parent process can read or write to the `USet`.
/// `keypos`: The index (1-indexed) that represents the key of the object. This function fails if this is greater than `size`.
///
/// Returns a result of the created `USet`, which can be used by other functions in this module.
///
pub fn new(
  name: Atom,
  size: Int,
  access: Access,
  keypos: Int,
) -> Result(USet, Option(ErlangError)) {
  case keypos > size || size < 2 {
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
          new_option.WriteConcurrency(new_option.Auto),
          new_option.ReadConcurrency(True),
          new_option.DecentralizedCounters(True),
        ])
      {
        Error(e) -> Error(Some(e))
        Ok(a) -> Ok(table.USet(a, size, keypos))
      }
    }
  }
}

/// Inserts a list of tuples into a `USet`.
///
/// Returns a `Bool` representing if the inserting succeeded. 
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the size of the tuple is less than the `USet`'s size.
///
/// If an `Object` with the same key already exists, then the old `Object` will be overwritten with the new one.
///
pub fn insert(uset: USet, objects: List(a)) -> Bool {
  bindings.uset_insert(uset, objects)
}

/// Inserts a list of `Object`s into a `USet`. It is recommended to use `insert` instead when possible, as this uses that function under the hood.
///
/// Returns a `Bool` representing if the inserting succeeded. 
/// - If `True`, all objects in the list were inserted.
/// - If `False`, _none_ of the objects in the list were inserted. This may occur if the size of the tuple is less than the `USet`'s size.
///
/// If an `Object` with the same key already exists, then the old `Object` will be overwritten with the new one.
///
pub fn insert_obj(uset: USet, objects: List(Object(a))) -> Bool {
  list.map(objects, object.extract)
  |> insert(uset, _)
}

/// Gets an `Object` from a `USet`.
///
/// Returns an `Option` containing the object, if it exists.
/// - If `Some`, then the object was found. ETS tables do not store types, so you must decode a `Dynamic` inside the `Object`.
/// - If `None`, then the `USet` did not contain any `Object` with the specified `key`.
///
pub fn lookup(uset: USet, key: a) -> Option(Object(Dynamic)) {
  case bindings.try_lookup(uset, key) {
    [res] -> Some(object.new(res))
    _ -> None
  }
}
