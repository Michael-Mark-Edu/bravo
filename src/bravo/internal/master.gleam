import bravo.{type Access, type BravoError}
import bravo/internal/bindings
import bravo/internal/new_option
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang.{type Reference}
import gleam/erlang/atom
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub type InnerTable {
  InnerTable(table: Reference, keypos: Int)
}

pub fn new(
  name name: String,
  keypos keypos: Int,
  access access: Access,
  ttype ttype: new_option.NewOption,
) -> Result(InnerTable, BravoError) {
  let atom = atom.create_from_string(name)
  use <- bool.guard(keypos < 1, Error(bravo.NonPositiveKeypos))
  use a <- result.try(
    bindings.try_new(atom, [
      ttype,
      case access {
        bravo.Public -> new_option.Public
        bravo.Protected -> new_option.Protected
        bravo.Private -> new_option.Private
      },
      new_option.NamedTable,
      new_option.Keypos(keypos),
      new_option.WriteConcurrency(new_option.Auto),
      new_option.ReadConcurrency(True),
      new_option.DecentralizedCounters(True),
    ]),
  )
  let assert Ok(tid) = bindings.try_whereis(a)
  Ok(InnerTable(tid, keypos))
}

pub fn insert(
  with table: InnerTable,
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  use <- bool.guard(list.is_empty(objects), Error(bravo.NothingToInsert))
  bindings.try_insert(table.table, table.keypos, objects)
}

pub fn insert_new(
  with table: InnerTable,
  insert objects: List(t),
) -> Result(Nil, BravoError) {
  use <- bool.guard(list.is_empty(objects), Error(bravo.NothingToInsert))
  case bindings.try_insert_new(table.table, table.keypos, objects) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(bravo.KeyAlreadyPresent)
    Error(e) -> Error(e)
  }
}

pub fn lookup_set(with table: InnerTable, at key: a) -> Result(t, BravoError) {
  use res <- result.try(bindings.try_lookup(table.table, key))
  case res {
    [a] -> Ok(a)
    _ -> Error(bravo.Empty)
  }
}

pub fn lookup_bag(
  with table: InnerTable,
  at key: a,
) -> Result(List(t), BravoError) {
  bindings.try_lookup(table.table, key)
}

pub fn delete(with table: InnerTable) -> Bool {
  bindings.try_delete(table.table)
}

pub fn delete_key(with table: InnerTable, at key: a) -> Nil {
  bindings.try_delete_key(table.table, key)
  Nil
}

pub fn delete_all_objects(with table: InnerTable) -> Nil {
  bindings.try_delete_all_objects(table.table)
  Nil
}

pub fn delete_object(with table: InnerTable, target object: t) -> Nil {
  bindings.try_delete_object(table.table, object)
  Nil
}

pub fn tab2file(
  with table: InnerTable,
  to filename: String,
  object_count object_count: Bool,
  md5sum md5sum: Bool,
  sync sync: Bool,
) -> Result(Nil, BravoError) {
  bindings.try_tab2file(
    table.table,
    string.to_utf_codepoints(filename),
    object_count,
    md5sum,
    sync,
  )
}

pub fn file2tab(
  from filename: String,
  verify verify: Bool,
  using decoder: fn(Dynamic) -> Result(t, _),
) -> Result(InnerTable, BravoError) {
  use name <- result.try(bindings.try_file2tab(
    string.to_utf_codepoints(filename),
    verify,
  ))
  let assert Ok(keypos) =
    dynamic.int(bindings.inform(name, atom.create_from_string("keypos")))
  let table = InnerTable(name, keypos)
  list.map(tab2list(table), fn(obj: t) {
    delete_object(table, obj)
    case bindings.tuple_size(obj) {
      1 -> insert(table, [bindings.element(1, obj)])
      _ -> insert(table, [obj])
    }
  })
  use <- bool.guard(
    {
      use obj <- list.all(tab2list(table))
      obj
      |> dynamic.from
      |> decoder
      |> result.is_ok
    },
    Ok(table),
  )
  delete(table)
  Error(bravo.DecodeFailure)
}

pub fn tab2list(with table: InnerTable) -> List(t) {
  bindings.try_tab2list(table.table)
}

pub fn take_set(with table: InnerTable, at key: a) -> Result(t, Nil) {
  case bindings.try_take(table.table, key) {
    [res] -> Ok(res)
    _ -> Error(Nil)
  }
}

pub fn take_bag(with table: InnerTable, at key: a) -> List(t) {
  bindings.try_take(table.table, key)
}

pub fn member(with table: InnerTable, at key: a) -> Bool {
  bindings.try_member(table.table, key)
}

pub fn first(with table: InnerTable) -> Result(a, Nil) {
  bindings.try_first(table.table)
}

pub fn last(with table: InnerTable) -> Result(a, Nil) {
  bindings.try_last(table.table)
}

pub fn next(with table: InnerTable, from key: a) -> Result(a, Nil) {
  bindings.try_next(table.table, key)
}

pub fn prev(with table: InnerTable, from key: a) -> Result(a, Nil) {
  bindings.try_prev(table.table, key)
}
