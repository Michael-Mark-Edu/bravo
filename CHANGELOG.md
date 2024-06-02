# v2.2.1
- Added checking in `insert` functions for if the input list is empty.
  Previously, inserting an empty list would cause an unintended runtime error.
  Now, the functions will simply return `False` when fed an empty list.

# v2.2.0
- Added `tab2file` to all tables.
- Added `file2tab` to all tables.
- Added `tab2list` to all tables.
- Changelog link in README.md is now on the version number instead of a separate
  label.
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
