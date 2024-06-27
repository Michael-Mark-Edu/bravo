//// Contains types used by all modules in the Bravo package.

/// The error type all Bravo functions use.
pub type BravoError {
  /// Used in cases where a function that can return an error `BravoError` is
  /// also able to return a non-value (aka `Nil`). In this scenario, `Nil` is
  /// replaced with `Empty`.
  Empty
  /// Thrown when accessing a key out-of-bounds using functions like `next`.
  EndOfTable
  /// Thrown if `file2tab` isn't able to successfully decode all elements in a
  /// table file.
  DecodeFailure
  /// Thrown when trying to operate on a non-existent table, whether it is
  /// because the table has not yet created or that it has been `delete`d.
  TableDoesNotExist
  /// Thrown when trying to create a table with the same name as an existing
  /// table.
  TableAlreadyExists
  /// Thrown when `insert_new` tries to insert with a key that the table already
  /// has.
  KeyAlreadyPresent
  /// Thrown when trying to perform an action on a table the current process
  /// does not have sufficient access to.
  ///
  /// (examples: `insert` on a `Protected` table, `lookup` on a `Private` table)
  AccessDenied
  /// Thrown when trying to perform an action on a table that has never been
  /// `insert`ed into. This library relies on a `insert`, `insert_new`, or
  /// `file2tab` call to initialize information about a table due to them
  /// requiring specifying the table's type. Not initializing a table will cause
  /// issues in the code, and as such is its own error type.
  UninitializedTable
  /// Runtime error caught in FFI. This is generally a fallthrough error type.
  /// A non-exhaustive list of possible causes:
  /// - `tab2file` couldn't create a file successfully.
  /// - `file2tab` tries to read non-ETS or corrupted data.
  ErlangError(String)
}

/// Access specifiers for ETS tables. Affects how other processes can interact
/// with the table.
pub type Access {
  /// Any process can read or write to the table.
  Public
  /// Any process can read the table. Only the owner process can write to it.
  Protected
  /// Only the owner process can read or write to the table.
  Private
}
