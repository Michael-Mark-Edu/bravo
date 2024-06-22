import bravo.{type BravoError}
import bravo/internal/new_option.{type NewOption}
import gleam/dynamic.{type Dynamic}
import gleam/erlang.{type Reference}
import gleam/erlang/atom.{type Atom}

@external(erlang, "bravo_ffi", "inform")
pub fn inform(table: Reference, key: Atom) -> Dynamic

@external(erlang, "bravo_ffi", "try_new")
pub fn try_new(
  table: Atom,
  options: List(NewOption),
) -> Result(Atom, BravoError)

@external(erlang, "bravo_ffi", "try_insert")
pub fn try_insert(table: Reference, key: k, value: v) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_insert_new")
pub fn try_insert_new(
  table: Reference,
  key: k,
  value: v,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_lookup")
pub fn try_lookup(table: Reference, key: a) -> Result(List(b), BravoError)

@external(erlang, "bravo_ffi", "try_take")
pub fn try_take(table: Reference, key: a) -> Result(List(b), BravoError)

@external(erlang, "bravo_ffi", "try_member")
pub fn try_member(table: Reference, key: a) -> Bool

@external(erlang, "bravo_ffi", "try_delete")
pub fn try_delete(table: Reference) -> Bool

@external(erlang, "bravo_ffi", "try_delete_key")
pub fn try_delete_key(table: Reference, key: a) -> Bool

@external(erlang, "bravo_ffi", "try_delete_object")
pub fn try_delete_object(table: Reference, object: a) -> Bool

@external(erlang, "bravo_ffi", "try_delete_all_objects")
pub fn try_delete_all_objects(table: Reference) -> Bool

@external(erlang, "bravo_ffi", "try_tab2file")
pub fn try_tab2file(
  table: Reference,
  filename: List(UtfCodepoint),
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_file2tab")
pub fn try_file2tab(
  filename: List(UtfCodepoint),
  verify: Bool,
) -> Result(Reference, BravoError)

@external(erlang, "bravo_ffi", "try_tab2list")
pub fn try_tab2list(table: Reference) -> Result(List(#(k, v)), BravoError)

@external(erlang, "bravo_ffi", "try_first")
pub fn try_first(table: Reference) -> Result(a, Nil)

@external(erlang, "bravo_ffi", "try_last")
pub fn try_last(table: Reference) -> Result(a, Nil)

@external(erlang, "bravo_ffi", "try_next")
pub fn try_next(table: Reference, key: a) -> Result(a, Nil)

@external(erlang, "bravo_ffi", "try_prev")
pub fn try_prev(table: Reference, key: a) -> Result(a, Nil)

@external(erlang, "bravo_ffi", "try_whereis")
pub fn try_whereis(atom: Atom) -> Result(Reference, Nil)

@external(erlang, "erlang", "tuple_size")
pub fn tuple_size(tuple: a) -> Int

@external(erlang, "erlang", "element")
pub fn element(n: Int, tuple: a) -> b

@external(erlang, "erlang", "is_tuple")
pub fn is_tuple(a: a) -> Bool
