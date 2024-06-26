//// Contains types used by all modules in the Bravo package.

import bravo/internal/new_options
import gleam/erlang/atom

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
  NoFilePermissions
  FileDoesNotExist
  InvalidPath
  ChecksumError
  InvalidObjectCount
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

pub type Spec {
  Spec(name: atom.Atom, opts: List(new_options.NewOption))
}

pub fn spec(name: String) -> Spec {
  Spec(atom.create_from_string(name), [])
}

pub fn access(spec spec: Spec, opt opt: Access) -> Spec {
  Spec(
    ..spec,
    opts: [
      case opt {
        Public -> new_options.Public
        Protected -> new_options.Protected
        Private -> new_options.Private
      },
      ..spec.opts
    ],
  )
}

pub fn write_concurrency(spec spec: Spec, opt opt: WriteConcurrency) -> Spec {
  let parsed = case opt {
    On -> new_options.True
    Off -> new_options.False
    Auto -> new_options.Auto
  }
  Spec(..spec, opts: [new_options.WriteConcurrency(parsed), ..spec.opts])
}

pub fn read_concurrency(spec spec: Spec, opt opt: Bool) -> Spec {
  Spec(..spec, opts: [new_options.ReadConcurrency(opt), ..spec.opts])
}

pub fn decentralized_counters(spec spec: Spec, opt opt: Bool) -> Spec {
  Spec(..spec, opts: [new_options.DecentralizedCounters(opt), ..spec.opts])
}

pub fn compressed(spec spec: Spec) -> Spec {
  Spec(..spec, opts: [new_options.Compressed, ..spec.opts])
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

pub type WriteConcurrency {
  On
  Off
  Auto
}
