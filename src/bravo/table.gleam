import gleam/erlang/atom.{type Atom}

/// An unordered set. Keys may only occur once per table, and objects are unordered.
///
/// In order for a lookup match to occur, entries must have the same value _and type_.
///
pub type USet {
  USet(table: Atom, keypos: Int)
}

/// An ordered set. Keys may only occur once per table, and objects are ordered (this comes at a performance cost).
///
/// In order for a lookup match to occur, entries must _coerce into the same value_. Two values may match even if they have different types.
///
pub type OSet {
  OSet(table: Atom, keypos: Int)
}

/// A bag table. Keys may occur multiple times per table, but objects cannot be copied verbatim.
///
pub type Bag {
  Bag(table: Atom, keypos: Int)
}

/// A duplicate bag table. Keys may occur multiple times per table, and verbatim copies of an object can be stored.
///
pub type DBag {
  DBag(table: Atom, keypos: Int)
}

/// Access specifiers for ETS tables. Affects how other processes can interact with the table.
pub type Access {
  /// Any process can read or write to the `USet`.
  Public
  /// Any process can read the `USet`. Only the owner process can write to it.
  Protected
  /// Only the parent process can read or write to the `USet`.
  Private
}
