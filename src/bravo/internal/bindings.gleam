import bravo/error.{type ErlangError}
import bravo/internal/new_option.{type NewOption}
import gleam/erlang/atom.{type Atom}

@external(erlang, "bravo", "try_new")
pub fn new(table: Atom, options: List(NewOption)) -> Result(Atom, ErlangError)

@external(erlang, "bravo", "try_insert")
pub fn try_insert(table: Atom, keypos: Int, list: List(a)) -> Bool

@external(erlang, "bravo", "try_lookup")
pub fn try_lookup(table: Atom, key: a) -> List(b)

@external(erlang, "bravo", "try_delete")
pub fn try_delete(table: Atom) -> Bool

@external(erlang, "erlang", "tuple_size")
pub fn tuple_size(tuple: a) -> Int

@external(erlang, "erlang", "element")
pub fn element(n: Int, tuple: a) -> b
