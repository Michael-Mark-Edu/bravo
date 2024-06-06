pub type BravoError {
  NonPositiveKeypos
  DecodeFailure
  ErlangError(String)
}

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
