import bravo.{type NewTableError}
import bravo/error.{type ErlangError}
import bravo/internal/new_option.{type NewOption}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}

@external(erlang, "bravo_ffi", "try_new")
pub fn try_new(
  table: Atom,
  options: List(NewOption),
) -> Result(Atom, NewTableError)

@external(erlang, "bravo_ffi", "try_insert")
pub fn try_insert(table: Atom, keypos: Int, list: List(a)) -> Bool

@external(erlang, "bravo_ffi", "try_lookup")
pub fn try_lookup(table: Atom, key: a) -> List(b)

@external(erlang, "bravo_ffi", "try_delete")
pub fn try_delete(table: Atom) -> Bool

@external(erlang, "bravo_ffi", "try_delete_key")
pub fn try_delete_key(table: Atom, key: a) -> Bool

@external(erlang, "bravo_ffi", "try_delete_all_objects")
pub fn try_delete_all_objects(table: Atom) -> Bool

@external(erlang, "bravo_ffi", "try_delete_object")
pub fn try_delete_object(table: Atom, object: a) -> Bool

@external(erlang, "bravo_ffi", "try_tab2file")
pub fn try_tab2file(
  table: Atom,
  filename: List(UtfCodepoint),
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> new_option.EmptyResult(ErlangError)

@external(erlang, "bravo_ffi", "try_file2tab")
pub fn try_file2tab(
  filename: List(UtfCodepoint),
  verify: Bool,
) -> Result(Atom, ErlangError)

@external(erlang, "bravo_ffi", "try_tab2list")
pub fn try_tab2list(table: Atom) -> a

@external(erlang, "bravo_ffi", "inform")
pub fn inform(table: Atom, key: Atom) -> Dynamic

@external(erlang, "bravo_ffi", "try_insert_new")
pub fn try_insert_new(table: Atom, keypos: Int, list: List(a)) -> Bool

@external(erlang, "bravo_ffi", "try_take")
pub fn try_take(table: Atom, key: a) -> List(b)

@external(erlang, "bravo_ffi", "try_member")
pub fn try_member(table: Atom, key: a) -> Bool

@external(erlang, "erlang", "tuple_size")
pub fn tuple_size(tuple: a) -> Int

@external(erlang, "erlang", "element")
pub fn element(n: Int, tuple: a) -> b

@external(erlang, "erlang", "is_tuple")
pub fn is_tuple(a: a) -> Bool
