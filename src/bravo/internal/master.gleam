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
  InnerTable(table: Reference)
}

pub fn new(
  name: String,
  access: Access,
  ttype: new_option.NewOption,
) -> Result(InnerTable, BravoError) {
  let atom = atom.create_from_string(name)
  use a <- result.try(
    bindings.try_new(atom, [
      ttype,
      case access {
        bravo.Public -> new_option.Public
        bravo.Protected -> new_option.Protected
        bravo.Private -> new_option.Private
      },
      new_option.NamedTable,
      new_option.Keypos(1),
      new_option.WriteConcurrency(new_option.Auto),
      new_option.ReadConcurrency(True),
      new_option.DecentralizedCounters(True),
    ]),
  )
  let assert Ok(tid) = bindings.try_whereis(a)
  Ok(InnerTable(tid))
}

pub fn insert(table: InnerTable, key: k, value: v) -> Result(Nil, BravoError) {
  bindings.try_insert(table.table, key, value)
}

pub fn insert_list(
  table: InnerTable,
  list: List(#(k, v)),
) -> Result(Nil, BravoError) {
  bindings.try_insert_list(table.table, list)
}

pub fn insert_new(
  table: InnerTable,
  key: k,
  value: v,
) -> Result(Nil, BravoError) {
  bindings.try_insert_new(table.table, key, value)
}

pub fn lookup_set(table: InnerTable, key: k) -> Result(v, BravoError) {
  use res <- result.try(bindings.try_lookup(table.table, key))
  let assert [a] = res
  Ok(a)
}

pub fn lookup_bag(table: InnerTable, key: k) -> Result(List(v), BravoError) {
  bindings.try_lookup(table.table, key)
}

pub fn take_set(table: InnerTable, key: k) -> Result(v, BravoError) {
  use res <- result.try(bindings.try_take(table.table, key))
  let assert [a] = res
  Ok(a)
}

pub fn take_bag(table: InnerTable, key: k) -> Result(List(v), BravoError) {
  bindings.try_take(table.table, key)
}

pub fn delete(table: InnerTable) -> Result(Nil, BravoError) {
  bindings.try_delete(table.table)
}

pub fn delete_key(table: InnerTable, key: a) -> Result(Nil, BravoError) {
  bindings.try_delete_key(table.table, key)
}

pub fn delete_all_objects(table: InnerTable) -> Result(Nil, BravoError) {
  bindings.try_delete_all_objects(table.table)
}

pub fn delete_object(
  table: InnerTable,
  key: k,
  value: v,
) -> Result(Nil, BravoError) {
  bindings.try_delete_object(table.table, #(key, value))
}

pub fn delete_object_tuple(
  table: InnerTable,
  object: #(k, v),
) -> Result(Nil, BravoError) {
  bindings.try_delete_object(table.table, object)
}

pub fn tab2file(
  table: InnerTable,
  filename: String,
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
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
  filename: String,
  verify: Bool,
  key_decode: fn(Dynamic) -> Result(k, _),
  value_decode: fn(Dynamic) -> Result(v, _),
) -> Result(InnerTable, BravoError) {
  use table <- result.try(bindings.try_file2tab(
    string.to_utf_codepoints(filename),
    verify,
  ))
  let assert Ok(objects) = bindings.try_tab2list(table)
  case
    {
      use object <- list.all(objects)
      let key =
        object.0
        |> key_decode
        |> result.is_ok
      let value =
        object.1
        |> value_decode
        |> result.is_ok
      bool.and(key, value)
    }
  {
    True -> Ok(InnerTable(table))
    False -> Error(bravo.DecodeFailure)
  }
}

pub fn tab2list(table: InnerTable) -> Result(List(#(k, v)), BravoError) {
  bindings.try_tab2list(table.table)
}

pub fn member(table: InnerTable, key: k) -> Result(Bool, BravoError) {
  bindings.try_member(table.table, key)
}

pub fn first(table: InnerTable) -> Result(k, BravoError) {
  bindings.try_first(table.table)
}

pub fn last(table: InnerTable) -> Result(k, BravoError) {
  bindings.try_last(table.table)
}

pub fn next(table: InnerTable, key: k) -> Result(k, BravoError) {
  bindings.try_next(table.table, key)
}

pub fn prev(table: InnerTable, key: k) -> Result(k, BravoError) {
  bindings.try_prev(table.table, key)
}
