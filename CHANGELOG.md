# v4.0.1 (unreleased)
- Changed the table types internally to store tids instead of atoms
    - This should result in better performance without changing the API.

# v4.0.0
This is a breaking update!
- Replaced _all_ instances where a function returns an `Option(x)` to instead
  return a `Result(x, Nil)`.
    - Thanks @lpil for bringing this to my attention. It is Gleam convention to
      return a `Result(x, Nil)` to signify a returned type can be nothing, and
      `Option(x)` is only used for function parameters that are nullable. In
      order to follow Gleam Convention, all `Option`s are now `Result`s.

# v3.2.0
- Added parameter labels to all functions.

# v3.1.0
- Added `first` to all tables.
- Added `last` to all tables.
- Added `next` to all tables.
- Added `prev` to all tables.

# v3.0.0
This is a breaking update!
- Complete overhaul of the error system. Instead of using `Option` or
  `ErlangError`, most failable functions are now `Result`s with error type
  `BravoError`. Affected functions are `new`, `tab2file`, and `file2tab`.
- All non-internal types and functions are now concentrated into five modules:
  four for each of the table types, and a new `bravo.gleam` module which acts
  like `bravo/error` and `bravo/etc` combined. Less imports should be required
  now.
- Changed how non-tuples are stored internally. It should now be more memory
  efficient to store non-tuples.
- Fixed issues with the interaction between non-tuple tables and `file2tab`.
- `file2tab` now deletes the table it creates if it fails to decode.

# v2.3.0
- Added `insert_new` to all tables.
- Added `member` to all tables.
- Added `take` to all tables.
- Refactoring of Erlang FFI to improve readability and remove unused code.
- Added "Bravo in 5 Minutes" section to `README.md`.
- This library is no longer "very much incomplete" so the notice in `README.md`
  has been removed.

# v2.2.1
- Added checking in `insert` functions for if the input list is empty.
  Previously, inserting an empty list would cause an unintended runtime error.
  Now, the functions will simply return `False` when fed an empty list.

# v2.2.0
- Added `tab2file` to all tables.
- Added `file2tab` to all tables.
- Added `tab2list` to all tables.
- Changelog link in `README.md` is now on the version number instead of a
  separate label.
- Edited changelog so that everything fits within 80 columns.

# v2.1.0
- Added `delete_key` to all tables.
- Added `delete_all_objects` to all tables.
- Added `delete_object` to all tables.
- Relaxed `gleam_erlang` version requirement to >= 0.23.1
- Relaxed `gleam_stdlib` version requirement to >= 0.33.0

# v2.0.0
This is a breaking update!
- `bravo/object` has been completely deleted.
- The table types are now typed. `insert` and `lookup` use these types now. As a
  result, tables are now restricted to only being able to store a single type of
  object, but now the library is significantly more type safe.
