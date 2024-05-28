//// This module provides types or functions that don't fit anywhere else.

/// Represents an Erlang `term()`, which can represent any data type.
///
pub type Term

/// Access specifiers for ETS tables. Affects how other processes can interact with the table.
///
pub type Access {
  /// Any process can read or write to the `USet`.
  Public
  /// Any process can read the `USet`. Only the owner process can write to it.
  Protected
  /// Only the parent process can read or write to the `USet`.
  Private
}
