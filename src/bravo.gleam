//// Contains types used by all modules in the Bravo package.

/// The error type all Bravo functions use.
///
pub type BravoError {
  /// Thrown if `new` is passed a `keypos` <= 0
  NonPositiveKeypos
  /// Thrown if `file2tab` isn't able to successfully decode all elements in a table file.
  DecodeFailure
  /// Runtime error caught in FFI. A non-exhaustive list of possible causes:
  /// - `new` tried to create a table with the same name as another.
  /// - `tab2file` couldn't create a file successfully.
  /// - `file2tab` tries to read non-ETS or corrupted data.
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
