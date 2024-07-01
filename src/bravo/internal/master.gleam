import bravo.{type Access, type BravoError}
import bravo/internal/bindings
import bravo/internal/new_options
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang.{type Reference}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/result
import gleam/string

pub type InnerTable {
  InnerTable(tid: Reference, atom: Atom)
}

pub fn new(
  name: String,
  access: Access,
  ttype: new_options.NewOption,
) -> Result(InnerTable, BravoError) {
  let atom = atom.create_from_string(name)
  use atom <- result.try(
    bindings.try_new(atom, [
      ttype,
      case access {
        bravo.Public -> new_options.Public
        bravo.Protected -> new_options.Protected
        bravo.Private -> new_options.Private
      },
      new_options.NamedTable,
      new_options.Keypos(1),
    ]),
  )
  let assert Ok(tid) = bindings.try_whereis(atom)
  Ok(InnerTable(tid, atom))
}

pub fn from_spec(
  spec: bravo.Spec,
  ttype: new_options.NewOption,
) -> Result(InnerTable, BravoError) {
  use atom <- result.try(
    bindings.try_new(spec.name, [
      ttype,
      new_options.NamedTable,
      new_options.Keypos(1),
      ..spec.opts
    ]),
  )
  let assert Ok(tid) = bindings.try_whereis(atom)
  Ok(InnerTable(tid, atom))
}

pub fn insert(table: InnerTable, key: k, value: v) -> Result(Nil, BravoError) {
  bindings.try_insert(table.tid, table.atom, key, value)
}

pub fn insert_list(
  table: InnerTable,
  list: List(#(k, v)),
) -> Result(Nil, BravoError) {
  bindings.try_insert_list(table.tid, table.atom, list)
}

pub fn insert_new(
  table: InnerTable,
  key: k,
  value: v,
) -> Result(Nil, BravoError) {
  case bindings.try_insert_new(table.tid, table.atom, key, value) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(bravo.KeyAlreadyPresent)
    Error(e) -> Error(e)
  }
}

pub fn insert_new_list(
  table: InnerTable,
  list: List(#(k, v)),
) -> Result(Nil, BravoError) {
  case bindings.try_insert_new_list(table.tid, table.atom, list) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(bravo.KeyAlreadyPresent)
    Error(e) -> Error(e)
  }
}

pub fn lookup_set(table: InnerTable, key: k) -> Result(v, BravoError) {
  use res <- result.try(bindings.try_lookup(table.tid, table.atom, key))
  let assert [a] = res
  Ok(a)
}

pub fn lookup_bag(table: InnerTable, key: k) -> Result(List(v), BravoError) {
  bindings.try_lookup(table.tid, table.atom, key)
}

pub fn take_set(table: InnerTable, key: k) -> Result(v, BravoError) {
  use res <- result.try(bindings.try_take(table.tid, table.atom, key))
  let assert [a] = res
  Ok(a)
}

pub fn take_bag(table: InnerTable, key: k) -> Result(List(v), BravoError) {
  bindings.try_take(table.tid, table.atom, key)
}

pub fn delete(table: InnerTable) -> Result(Nil, BravoError) {
  bindings.try_delete(table.tid, table.atom)
}

pub fn delete_key(table: InnerTable, key: a) -> Result(Nil, BravoError) {
  bindings.try_delete_key(table.tid, table.atom, key)
}

pub fn delete_all_objects(table: InnerTable) -> Result(Nil, BravoError) {
  bindings.try_delete_all_objects(table.tid, table.atom)
}

pub fn delete_object(
  table: InnerTable,
  key: k,
  value: v,
) -> Result(Nil, BravoError) {
  bindings.try_delete_object(table.tid, table.atom, #(key, value))
}

pub fn delete_object_tuple(
  table: InnerTable,
  object: #(k, v),
) -> Result(Nil, BravoError) {
  bindings.try_delete_object(table.tid, table.atom, object)
}

pub fn tab2file(
  table: InnerTable,
  filename: String,
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Result(Nil, BravoError) {
  bindings.try_tab2file(
    table.tid,
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
  use atom <- result.try(bindings.try_file2tab(
    string.to_utf_codepoints(filename),
    verify,
  ))
  let assert Ok(table) = bindings.try_whereis(atom)
  let assert Ok(objects) = bindings.try_tab2list(table, atom)
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
    True -> Ok(InnerTable(table, atom))
    False -> {
      let _ = bindings.try_delete(table, atom)
      Error(bravo.DecodeFailure)
    }
  }
}

pub fn tab2list(table: InnerTable) -> Result(List(#(k, v)), BravoError) {
  bindings.try_tab2list(table.tid, table.atom)
}

pub fn member(table: InnerTable, key: k) -> Result(Bool, BravoError) {
  bindings.try_member(table.tid, table.atom, key)
}

pub fn first(table: InnerTable) -> Result(k, BravoError) {
  bindings.try_first(table.tid, table.atom)
}

pub fn last(table: InnerTable) -> Result(k, BravoError) {
  bindings.try_last(table.tid, table.atom)
}

pub fn next(table: InnerTable, key: k) -> Result(k, BravoError) {
  bindings.try_next(table.tid, table.atom, key)
}

pub fn prev(table: InnerTable, key: k) -> Result(k, BravoError) {
  bindings.try_prev(table.tid, table.atom, key)
}
