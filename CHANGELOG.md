# v2.1.0
- Added `delete_key` to all tables.
- Added `delete_all_objects` to all tables.
- Added `delete_object` to all tables.

# v2.0.0
This is a breaking update!

- `bravo/object` has been completely deleted.
- The table types are now typed. `insert` and `lookup` use these types now. As a result, tables are
  now restricted to only being able to store a single type of object, but now the library is
  significantly more type safe.
