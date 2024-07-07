import bravo.{type BravoError}
import bravo/internal
import bravo/internal/new_options
import gleam/erlang.{type Reference}
import gleam/erlang/atom.{type Atom}

@external(erlang, "bravo_ffi", "try_new")
pub fn try_new(
  atom: Atom,
  options: List(new_options.NewOption),
) -> Result(Atom, BravoError)

@external(erlang, "bravo_ffi", "try_insert")
pub fn try_insert(
  tid: Reference,
  atom: Atom,
  key: k,
  value: v,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_insert_list")
pub fn try_insert_list(
  tid: Reference,
  atom: Atom,
  list: List(#(k, v)),
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_insert_new")
pub fn try_insert_new(
  tid: Reference,
  atom: Atom,
  key: k,
  value: v,
) -> Result(Bool, BravoError)

@external(erlang, "bravo_ffi", "try_insert_new_list")
pub fn try_insert_new_list(
  tid: Reference,
  atom: Atom,
  list: List(#(k, v)),
) -> Result(Bool, BravoError)

@external(erlang, "bravo_ffi", "try_lookup")
pub fn try_lookup(
  tid: Reference,
  atom: Atom,
  key: a,
) -> Result(List(b), BravoError)

@external(erlang, "bravo_ffi", "try_take")
pub fn try_take(
  tid: Reference,
  atom: Atom,
  key: a,
) -> Result(List(b), BravoError)

@external(erlang, "bravo_ffi", "try_member")
pub fn try_member(
  tid: Reference,
  atom: Atom,
  key: a,
) -> Result(Bool, BravoError)

@external(erlang, "bravo_ffi", "try_delete")
pub fn try_delete(tid: Reference, atom: Atom) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_delete_key")
pub fn try_delete_key(
  tid: Reference,
  atom: Atom,
  key: a,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_delete_object")
pub fn try_delete_object(
  tid: Reference,
  atom: Atom,
  object: a,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_delete_all_objects")
pub fn try_delete_all_objects(
  tid: Reference,
  atom: Atom,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_tab2file")
pub fn try_tab2file(
  tid: Reference,
  filename: List(UtfCodepoint),
  object_count: Bool,
  md5sum: Bool,
  sync: Bool,
) -> Result(Nil, BravoError)

@external(erlang, "bravo_ffi", "try_file2tab")
pub fn try_file2tab(
  filename: List(UtfCodepoint),
  verify: Bool,
) -> Result(Atom, BravoError)

@external(erlang, "bravo_ffi", "try_tab2list")
pub fn try_tab2list(
  tid: Reference,
  atom: Atom,
) -> Result(List(#(k, v)), BravoError)

@external(erlang, "bravo_ffi", "try_first")
pub fn try_first(tid: Reference, atom: Atom) -> Result(a, BravoError)

@external(erlang, "bravo_ffi", "try_last")
pub fn try_last(tid: Reference, atom: Atom) -> Result(a, BravoError)

@external(erlang, "bravo_ffi", "try_next")
pub fn try_next(tid: Reference, atom: Atom, key: a) -> Result(a, BravoError)

@external(erlang, "bravo_ffi", "try_prev")
pub fn try_prev(tid: Reference, atom: Atom, key: a) -> Result(a, BravoError)

@external(erlang, "bravo_ffi", "try_whereis")
pub fn try_whereis(atom: Atom) -> Result(Reference, Nil)
