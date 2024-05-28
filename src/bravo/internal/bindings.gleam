import bravo/error.{type ErlangError}
import bravo/internal/new_option.{type NewOption}
import bravo/table.{type USet}
import gleam/erlang/atom.{type Atom}

@external(erlang, "bravo", "try_new")
pub fn new(table: Atom, options: List(NewOption)) -> Result(Atom, ErlangError)

@external(erlang, "bravo", "try_insert")
pub fn uset_insert(set: USet, list: List(a)) -> Bool

@external(erlang, "bravo", "try_lookup")
pub fn try_lookup(set: USet, key: a) -> List(b)

@external(erlang, "bravo", "try_delete")
pub fn uset_delete(uset: USet) -> Bool

@external(erlang, "erlang", "tuple_size")
pub fn tuple_size(tuple: a) -> Int

@external(erlang, "erlang", "element")
pub fn element(n: Int, tuple: a) -> b
