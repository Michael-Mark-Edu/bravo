import bravo/error.{type ErlangError}
import bravo/internal/new_option.{type NewOption}
import bravo/object.{type Object}
import bravo/table.{type Set}
import gleam/erlang/atom.{type Atom}
import gleam/option.{type Option}

@external(erlang, "bravo", "try_new")
pub fn new(table: Atom, options: List(NewOption)) -> Result(Atom, ErlangError)

@external(erlang, "bravo", "try_insert")
pub fn set_insert(set: Set, list: List(Object(a))) -> Bool

@external(erlang, "bravo", "set_lookup")
pub fn set_lookup(set: Set, key: a) -> Option(Object(b))
