// import bravo
// import bravo/dbag
// import bravo/internal/bindings
// import gleam/dict
// import gleam/dynamic
// import gleam/erlang/atom
// import gleam/erlang/process
// import gleam/io
// import gleam/list
// import gleam/option
// import gleam/otp/actor
// import gleam/otp/task
// import gleeunit/should
// import simplifile
//
// fn defer(defer: fn() -> a, block: fn() -> b) -> b {
//   let b = block()
//   defer()
//   b
// }
//
// pub fn dbag_insert_lookup_delete_test() {
//   let assert Ok(table) = dbag.new("dbag1", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(100, 200), #(300, 500)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 100)
//   |> should.equal(Ok([#(100, 200)]))
//   dbag.lookup(table, 300)
//   |> should.equal(Ok([#(300, 500)]))
//   dbag.lookup(table, 600)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_multitype_test() {
//   let assert Ok(table) = dbag.new("dbag2", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("a", 1), #("b", 2)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, "a")
//   |> should.equal(Ok([#("a", 1)]))
//   dbag.lookup(table, "b")
//   |> should.equal(Ok([#("b", 2)]))
//   dbag.lookup(table, "c")
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_large_test() {
//   let assert Ok(table) = dbag.new("dbag3", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [
//     #(
//       900,
//       800,
//       700,
//       600,
//       500,
//       400,
//       300,
//       200,
//       100,
//       0,
//       -100,
//       -200,
//       -300,
//       -400,
//       -500,
//       -600,
//       -700,
//       -800,
//       -900,
//     ),
//   ])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 900)
//   |> should.equal(
//     Ok([
//       #(
//         900,
//         800,
//         700,
//         600,
//         500,
//         400,
//         300,
//         200,
//         100,
//         0,
//         -100,
//         -200,
//         -300,
//         -400,
//         -500,
//         -600,
//         -700,
//         -800,
//         -900,
//       ),
//     ]),
//   )
// }
//
// pub fn dbag_keypos_test() {
//   let assert Ok(table) = dbag.new("dbag4", 2, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(100, 200), #(300, 500)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 200)
//   |> should.equal(Ok([#(100, 200)]))
//   dbag.lookup(table, 500)
//   |> should.equal(Ok([#(300, 500)]))
//   dbag.lookup(table, 100)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_bad_new_test() {
//   let assert Ok(table) = dbag.new("dbag5", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.new("dbag5", 1, bravo.Public)
//   |> should.equal(Error(bravo.TableAlreadyExists))
// }
//
// pub fn dbag_bad_insert_test() {
//   let assert Ok(table) = dbag.new("dbag6", 3, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("a", 1)])
//   |> should.equal(Error(bravo.InvalidKeypos))
// }
//
// pub fn dbag_multi_insert_test() {
//   let assert Ok(table) = dbag.new("dbag7", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(100, 200)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 100)
//   |> should.equal(Ok([#(100, 200)]))
//   dbag.insert(table, [#(100, 300), #(100, 400), #(100, 400)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 100)
//   |> should.equal(Ok([#(100, 200), #(100, 300), #(100, 400), #(100, 400)]))
// }
//
// pub fn dbag_large_multitype_test() {
//   let assert Ok(table) = dbag.new("dbag8", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [
//     #(
//       "String",
//       5,
//       10.0,
//       [15, 20],
//       #(25, 30),
//       dict.from_list([#(35, 40)]),
//       Ok(45),
//       Ok(50),
//     ),
//   ])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, "String")
//   |> should.equal(
//     Ok([
//       #(
//         "String",
//         5,
//         10.0,
//         [15, 20],
//         #(25, 30),
//         dict.from_list([#(35, 40)]),
//         Ok(45),
//         Ok(50),
//       ),
//     ]),
//   )
// }
//
// pub fn dbag_delete_test() {
//   let assert Ok(table) = dbag.new("dbag9", 1, bravo.Public)
//   dbag.delete(table)
//   |> should.equal(True)
//   let assert Ok(table) = dbag.new("dbag9", 1, bravo.Public)
//   dbag.delete(table)
//   |> should.equal(True)
//   dbag.delete(table)
//   |> should.equal(False)
// }
//
// pub fn dbag_singleton_test() {
//   let assert Ok(table) = dbag.new("dbag10", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(1), #(2)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 1)
//   |> should.equal(Ok([#(1)]))
//   dbag.lookup(table, 2)
//   |> should.equal(Ok([#(2)]))
// }
//
// pub fn dbag_nontuple_test() {
//   let assert Ok(table) = dbag.new("dbag11", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [5])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 5)
//   |> should.equal(Ok([5]))
// }
//
// pub fn dbag_nontuple_record_test() {
//   let assert Ok(table) = dbag.new("dbag12", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [Ok(5)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, Ok(5))
//   |> should.equal(Ok([Ok(5)]))
// }
//
// type Multirecord {
//   A(Int)
//   B(Int, Int)
//   C
// }
//
// pub fn dbag_nontuple_multirecord_test() {
//   let assert Ok(table) = dbag.new("dbag14", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [A(1), B(2, 3), C])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, A(1))
//   |> should.equal(Ok([A(1)]))
//   dbag.lookup(table, B(2, 3))
//   |> should.equal(Ok([B(2, 3)]))
//   dbag.lookup(table, C)
//   |> should.equal(Ok([C]))
// }
//
// pub fn dbag_delete_key_test() {
//   let assert Ok(table) = dbag.new("dbag14", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, "Bye")
//   |> should.equal(Ok([#("Bye", "World"), #("Bye", "Bye")]))
//   dbag.delete_key(table, "Bye")
//   dbag.lookup(table, "Bye")
//   |> should.equal(Error(bravo.Empty))
//   dbag.lookup(table, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
// }
//
// pub fn dbag_delete_all_objects_test() {
//   let assert Ok(table) = dbag.new("dbag15", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
//   |> should.equal(Ok(Nil))
//   dbag.delete_all_objects(table)
//   dbag.lookup(table, "Hello")
//   |> should.equal(Error(bravo.Empty))
//   dbag.lookup(table, "Bye")
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_delete_object_test() {
//   let assert Ok(table) = dbag.new("dbag16", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [
//     #("Hello", "World"),
//     #("Bye", "World"),
//     #("Bye", "Bye"),
//     #("Bye", "Bye"),
//   ])
//   |> should.equal(Ok(Nil))
//   dbag.delete_object(table, #("Bye", "Bye"))
//   dbag.lookup(table, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   dbag.lookup(table, "Bye")
//   |> should.equal(Ok([#("Bye", "World")]))
// }
//
// pub fn dbag_tab2file_test() {
//   let assert Ok(table) = dbag.new("dbag17", 2, bravo.Public)
//   dbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   dbag.tab2file(table, "dbag17", True, True, True)
//   |> should.equal(Ok(Nil))
//   dbag.delete(table)
//   let assert Ok(new_table) =
//     dbag.file2tab(
//       "dbag17",
//       True,
//       dynamic.tuple2(dynamic.string, dynamic.string),
//     )
//   dbag.lookup(new_table, "World")
//   |> should.equal(Ok([#("Hello", "World")]))
//   dbag.delete(new_table)
//   dbag.file2tab("dbag17", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("dbag17")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn dbag_tab2list_test() {
//   let assert Ok(table) = dbag.new("dbag18", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.equal(Ok(Nil))
//   let objects = dbag.tab2list(table)
//   list.contains(objects, #("Hello", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "Bye"))
//   |> should.equal(False)
// }
//
// pub fn dbag_tab2list_orderedness_test() {
//   let assert Ok(table) = dbag.new("dbag19", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [
//     #("A"),
//     #("Q"),
//     #("C"),
//     #("R"),
//     #("Z"),
//     #("B"),
//     #("S"),
//     #("F"),
//     #("Da"),
//     #("DA"),
//     #("Db"),
//     #("a"),
//   ])
//   |> should.equal(Ok(Nil))
//   dbag.tab2list(table)
//   |> should.not_equal([
//     #("A"),
//     #("Q"),
//     #("C"),
//     #("R"),
//     #("Z"),
//     #("B"),
//     #("S"),
//     #("F"),
//     #("Da"),
//     #("DA"),
//     #("Db"),
//     #("a"),
//   ])
// }
//
// pub fn dbag_empty_insert_test() {
//   let assert Ok(table) = dbag.new("dbag20", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [])
//   |> should.equal(Error(bravo.NothingToInsert))
// }
//
// pub fn dbag_dynamic_test() {
//   let assert Ok(table) = dbag.new("dbag21", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [
//     dynamic.from(#("Hello", "World")),
//     dynamic.from(#("Hello", "my", "friend!")),
//     dynamic.from(#("Hello", "World")),
//     dynamic.from(#(1, 2, 3)),
//   ])
//   |> should.equal(Ok(Nil))
//   let assert Ok(list) = dbag.lookup(table, "Hello")
//   list.contains(list, dynamic.from(#("Hello", "World")))
//   |> should.equal(True)
//   let assert Ok(#(_, newlist)) = {
//     use a <- list.pop(list)
//     let fun = dynamic.tuple2(dynamic.string, dynamic.string)
//     let assert Ok(res) = fun(a)
//     res == #("Hello", "World")
//   }
//   list.contains(newlist, dynamic.from(#("Hello", "World")))
//   |> should.equal(True)
//   list.contains(newlist, dynamic.from(#("Hello", "my", "friend!")))
//   |> should.equal(True)
//   dbag.lookup(table, 1)
//   |> should.equal(Ok([dynamic.from(#(1, 2, 3))]))
// }
//
// pub fn dbag_insert_new_test() {
//   let assert Ok(table) = dbag.new("dbag22", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert_new(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   dbag.insert_new(table, [#(1, 3), #(2, 4)])
//   |> should.equal(Error(bravo.KeyAlreadyPresent))
//   dbag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   dbag.lookup(table, 2)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_take_test() {
//   let assert Ok(table) = dbag.new("dbag23", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   dbag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   dbag.take(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   dbag.take(table, 1)
//   |> should.equal(Error(bravo.Empty))
//   dbag.lookup(table, 1)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn dbag_member_test() {
//   let assert Ok(table) = dbag.new("dbag24", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   dbag.member(table, 1)
//   |> should.equal(True)
//   dbag.member(table, 2)
//   |> should.equal(False)
//   dbag.delete_key(table, 1)
//   dbag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn dbag_singleton_member_test() {
//   let assert Ok(table) = dbag.new("dbag25", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [1, 3])
//   |> should.equal(Ok(Nil))
//   dbag.member(table, 1)
//   |> should.equal(True)
//   dbag.member(table, 2)
//   |> should.equal(False)
//   dbag.delete_key(table, 1)
//   dbag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn dbag_tab2file_singleton_test() {
//   let assert Ok(table) = dbag.new("dbag26", 1, bravo.Public)
//   dbag.insert(table, ["Hello"])
//   |> should.equal(Ok(Nil))
//   dbag.tab2file(table, "dbag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   dbag.delete(table)
//   let assert Ok(new_table) = dbag.file2tab("dbag26", True, dynamic.string)
//   dbag.lookup(new_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   dbag.delete(new_table)
//   dbag.file2tab("dbag26", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//
//   let assert Ok(newer_table) = dbag.new("dbag26", 1, bravo.Public)
//   dbag.insert(newer_table, [#("Hello")])
//   |> should.equal(Ok(Nil))
//   dbag.tab2file(newer_table, "dbag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   dbag.delete(newer_table)
//   let assert Ok(newest_table) = dbag.file2tab("dbag26", True, dynamic.string)
//   dbag.lookup(newest_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   dbag.delete(newest_table)
//
//   simplifile.delete("dbag26")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn dbag_tab2file_singleton_record_test() {
//   let assert Ok(table) = dbag.new("dbag27", 1, bravo.Public)
//   dbag.insert(table, [Ok("Hello"), Error("World")])
//   |> should.equal(Ok(Nil))
//   dbag.tab2file(table, "dbag27", True, True, True)
//   |> should.equal(Ok(Nil))
//   dbag.delete(table)
//   let assert Ok(new_table) =
//     dbag.file2tab(
//       "dbag27",
//       True,
//       dynamic.result(dynamic.string, dynamic.string),
//     )
//   dbag.lookup(new_table, Ok("Hello"))
//   |> should.equal(Ok([Ok("Hello")]))
//   dbag.lookup(new_table, Error("World"))
//   |> should.equal(Ok([Error("World")]))
//   dbag.delete(new_table)
//   dbag.file2tab("dbag27", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("dbag27")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn dbag_fn_test() {
//   let assert Ok(table) = dbag.new("dbag28", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   let dataset = [
//     "A", "Q", "C", "R", "Z", "Z", "B", "S", "F", "Da", "DA", "Db", "a",
//   ]
//   dbag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> dbag.first
//   let assert Ok(a) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(b) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(c) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(d) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(e) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(f) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(g) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(h) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(i) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(j) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(k) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.next(key)
//   let assert Ok(l) = dbag.lookup(table, key)
//   let list = []
//   let list = list.append(a, list)
//   let list = list.append(b, list)
//   let list = list.append(c, list)
//   let list = list.append(d, list)
//   let list = list.append(e, list)
//   let list = list.append(f, list)
//   let list = list.append(g, list)
//   let list = list.append(h, list)
//   let list = list.append(i, list)
//   let list = list.append(j, list)
//   let list = list.append(k, list)
//   let list = list.append(l, list)
//   list.map(dataset, fn(elem) {
//     list.contains(list, elem)
//     |> should.equal(True)
//   })
//   table |> dbag.next(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// pub fn dbag_lp_test() {
//   let assert Ok(table) = dbag.new("dbag29", 1, bravo.Public)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   let dataset = [
//     "A", "Q", "C", "R", "Z", "Z", "B", "S", "F", "Da", "DA", "Db", "a",
//   ]
//   dbag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> dbag.last
//   let assert Ok(a) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(b) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(c) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(d) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(e) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(f) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(g) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(h) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(i) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(j) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(k) = dbag.lookup(table, key)
//   let assert Ok(key) = table |> dbag.prev(key)
//   let assert Ok(l) = dbag.lookup(table, key)
//   let list = []
//   let list = list.append(a, list)
//   let list = list.append(b, list)
//   let list = list.append(c, list)
//   let list = list.append(d, list)
//   let list = list.append(e, list)
//   let list = list.append(f, list)
//   let list = list.append(g, list)
//   let list = list.append(h, list)
//   let list = list.append(i, list)
//   let list = list.append(j, list)
//   let list = list.append(k, list)
//   let list = list.append(l, list)
//   list.map(dataset, fn(elem) {
//     list.contains(list, elem)
//     |> should.equal(True)
//   })
//   table |> dbag.prev(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// // TODO: Replace the internal binding calls here with actual functions
// pub fn dbag_async_access_test() {
//   let assert Ok(table) = dbag.new("dbag30", 1, bravo.Private)
//   use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
//   dbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   let assert Ok(ref) =
//     "dbag30"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   let task = {
//     use <- task.async
//     let assert Ok(ref) =
//       "dbag30"
//       |> atom.create_from_string
//       |> bindings.try_whereis
//     bindings.try_lookup(ref, "Hello")
//     |> should.equal(Error(bravo.AccessDenied))
//   }
//   task.await_forever(task)
// }
//
// pub fn dbag_async_protected_test() {
//   let assert Ok(actor) = {
//     use _, _ <- actor.start(option.None)
//     let assert Ok(table) = dbag.new("dbag30a", 1, bravo.Protected)
//     dbag.insert(table, [#("Goodbye", "World")])
//     |> should.equal(Ok(Nil))
//     actor.Continue(option.Some(table), option.None)
//   }
//   actor.send(actor, Nil)
//   process.sleep(100)
//   let assert Ok(ref) =
//     "dbag30a"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Goodbye")
//   |> should.equal(Ok([#("Goodbye", "World")]))
//   bindings.try_insert(ref, 1, [#("Hello", "Again")])
//   |> should.equal(Error(bravo.AccessDenied))
// }
//
// pub fn dbag_recreation_test() {
//   let assert Ok(table) = dbag.new("dbag31", 1, bravo.Private)
//   dbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   dbag.delete(table)
//   let assert Ok(_table2) = dbag.new("dbag31", 1, bravo.Private)
//   dbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Error(bravo.TableDoesNotExist))
// }
