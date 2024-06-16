//// Contains types used by all modules in the Bravo package.

/// The error type all Bravo functions use.
pub type BravoError {
  /// Thrown if `new` is passed a `keypos` <= 0
  NonPositiveKeypos
  /// Thrown if you try to `insert` an object with less elements than `keypos`.
  InvalidKeypos
  /// Thrown if `file2tab` isn't able to successfully decode all elements in a
  /// table file.
  DecodeFailure
  /// Thrown when trying to operate on a non-existent table, whether it is
  /// because the table has not yet created or that it has been `delete`d.
  TableDoesNotExist
  /// Thrown when `insert` and similar functions are passed an empty list.
  NothingToInsert
  /// Thrown when trying to perform an action on a table the current process
  /// does not have sufficient access to.
  ///
  /// (examples: `insert` on a `Protected` table, `lookup` on a `Private` table)
  AccessDenied
  /// Runtime error caught in FFI. This is generally a fallthrough error type.
  /// A non-exhaustive list of possible causes:
  /// - `new` tried to create a table with the same name as another.
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
