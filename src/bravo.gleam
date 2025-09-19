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
  /// Thrown when trying to write to a file the program doesn't have write
  /// access to.
  NoFilePermissions
  /// The file at the given filepath does not exist or otherwise could not be
  /// found.
  FileDoesNotExist
  /// Do all of the directories in the path exist?
  InvalidPath
  /// Thrown when `file2tab` detects a mismatched checksum in a table file
  /// created by `tab2file` with `md5sum` set to `True`.
  ChecksumError
  /// Thrown when `file2tab` detects a mismatched object count in a table file
  /// created by `tab2file` with `object_count` set to `True`.
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

/// A type with all the information needed to create a table.
pub type Spec {
  Spec(name: atom.Atom, opts: List(new_options.NewOption))
}

/// Creates a `Spec` with a name.
///
/// # Parameters
/// - `name name: String`
/// The name of the table to create.
///
/// # Returns
/// - `Spec`
/// The created `Spec`.
pub fn spec(name name: String) -> Spec {
  Spec(atom.create(name), [])
}

/// Sets a `Spec`'s access control.
///
/// When this function is not called, defaults to `Protected`.
///
/// # Parameter
/// - `spec spec: Spec`
/// The spec to add to.
/// - `opt opt: Access`
/// The access level to set. See docs for `bravo.Access`.
///
/// # Returns
/// - `Spec`
/// The modified `Spec`.
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

/// Sets a `Spec`'s write concurrency flag. ///
/// When this function is not called, defaults to `Off`.
///
/// # Parameter
/// - `spec spec: Spec`
/// The spec to add to.
/// - `opt opt: WriteConcurrency`
/// The write concurrency flag to set. See `bravo.WriteConcurrency` for more
/// info.
///
/// # Returns
/// - `Spec`
/// The modified `Spec`.
pub fn write_concurrency(spec spec: Spec, opt opt: WriteConcurrency) -> Spec {
  let parsed = case opt {
    On -> new_options.True
    Off -> new_options.False
    Auto -> new_options.Auto
  }
  Spec(..spec, opts: [new_options.WriteConcurrency(parsed), ..spec.opts])
}

/// Sets a `Spec`'s read concurrency flag. When `True`, read operations are
/// cheaper, but switching between reading and writing is more expensive. Enable
/// if reads are much more common than writes, or if reads and writes come in
/// large bursts of each respectively.
///
/// When this function is not called, defaults to `False`.
///
/// # Parameter
/// - `spec spec: Spec`
/// The spec to add to.
/// - `opt opt: Bool`
/// The read concurrency flag to set.
///
/// # Returns
/// - `Spec`
/// The modified `Spec`.
pub fn read_concurrency(spec spec: Spec, opt opt: Bool) -> Spec {
  Spec(..spec, opts: [new_options.ReadConcurrency(opt), ..spec.opts])
}

/// Sets a `Spec`'s decentralized counters flag. Improves performance of actions
/// which modify the table's size in memory (i.e. `insert`, `delete_key`) at the
/// cost of making it much slower to get the size or memory usage of the table.
/// In most cases it is recommended to enable this.
///
/// When this function is not called, defaults to `False`, unless
/// `write_concurrency` is set to `Auto` (or, in the case of an `OSet` also when
/// set to `On`) in which case it will default to `True`.
///
/// # Parameter
/// - `spec spec: Spec`
/// The spec to add to.
/// - `opt opt: Bool`
/// The read concurrency flag to set.
///
/// # Returns
/// - `Spec`
/// The modified `Spec`.
pub fn decentralized_counters(spec spec: Spec, opt opt: Bool) -> Spec {
  Spec(..spec, opts: [new_options.DecentralizedCounters(opt), ..spec.opts])
}

/// Sets a `Spec`'s compression flag. Table data is compressed at the cost of
/// major performance degredation. Use if memory usage is a bigger concern than
/// performance.
///
/// Disabled if this function is not called, enabled if it is.
///
/// # Parameters
/// - `spec spec: Spec`
/// The spec to add to.
///
/// # Returns
/// - `Spec`
/// The modified `Spec`.
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

/// Trinary options used for `write_concurrency`.
pub type WriteConcurrency {
  /// Different objects may be modified at the same time concurrently.
  On
  /// All writes to the table are blocking.
  Off
  /// Like `On`, except with granularity optimizations that help in most cases.
  Auto
}
