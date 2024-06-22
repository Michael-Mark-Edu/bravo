// import bravo
// import bravo/internal/bindings
// import bravo/oset
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
// pub fn oset_insert_lookup_delete_test() {
//   let assert Ok(table) = oset.new("oset1", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(100, 200), #(300, 500)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 100)
//   |> should.equal(Ok(#(100, 200)))
//   oset.lookup(table, 300)
//   |> should.equal(Ok(#(300, 500)))
//   oset.lookup(table, 600)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_multitype_test() {
//   let assert Ok(table) = oset.new("oset2", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("a", 1), #("b", 2)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, "a")
//   |> should.equal(Ok(#("a", 1)))
//   oset.lookup(table, "b")
//   |> should.equal(Ok(#("b", 2)))
//   oset.lookup(table, "c")
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_large_test() {
//   let assert Ok(table) = oset.new("oset3", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
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
//   oset.lookup(table, 900)
//   |> should.equal(
//     Ok(#(
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
//     )),
//   )
// }
//
// pub fn oset_keypos_test() {
//   let assert Ok(table) = oset.new("oset4", 2, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(100, 200), #(300, 500)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 200)
//   |> should.equal(Ok(#(100, 200)))
//   oset.lookup(table, 500)
//   |> should.equal(Ok(#(300, 500)))
//   oset.lookup(table, 100)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_bad_new_test() {
//   let assert Ok(table) = oset.new("oset5", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.new("oset5", 1, bravo.Public)
//   |> should.equal(Error(bravo.TableAlreadyExists))
// }
//
// pub fn oset_bad_insert_test() {
//   let assert Ok(table) = oset.new("oset6", 3, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("a", 1)])
//   |> should.equal(Error(bravo.InvalidKeypos))
// }
//
// pub fn oset_multi_insert_test() {
//   let assert Ok(table) = oset.new("oset7", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(100, 200)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 100)
//   |> should.equal(Ok(#(100, 200)))
//   oset.insert(table, [#(100, 300), #(100, 400)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 100)
//   |> should.equal(Ok(#(100, 400)))
// }
//
// pub fn oset_large_multitype_test() {
//   let assert Ok(table) = oset.new("oset8", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
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
//   oset.lookup(table, "String")
//   |> should.equal(
//     Ok(#(
//       "String",
//       5,
//       10.0,
//       [15, 20],
//       #(25, 30),
//       dict.from_list([#(35, 40)]),
//       Ok(45),
//       Ok(50),
//     )),
//   )
// }
//
// pub fn oset_delete_test() {
//   let assert Ok(table) = oset.new("oset9", 1, bravo.Public)
//   oset.delete(table)
//   |> should.equal(True)
//   let assert Ok(table) = oset.new("oset9", 1, bravo.Public)
//   oset.delete(table)
//   |> should.equal(True)
//   oset.delete(table)
//   |> should.equal(False)
// }
//
// pub fn oset_singleton_test() {
//   let assert Ok(table) = oset.new("oset10", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(1), #(2)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 1)
//   |> should.equal(Ok(#(1)))
//   oset.lookup(table, 2)
//   |> should.equal(Ok(#(2)))
// }
//
// pub fn oset_nontuple_test() {
//   let assert Ok(table) = oset.new("oset11", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [5])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 5)
//   |> should.equal(Ok(5))
// }
//
// pub fn oset_nontuple_record_test() {
//   let assert Ok(table) = oset.new("oset12", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [Ok(5)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, Ok(5))
//   |> should.equal(Ok(Ok(5)))
// }
//
// type Multirecord {
//   A(Int)
//   B(Int, Int)
//   C
// }
//
// pub fn oset_nontuple_multirecord_test() {
//   let assert Ok(table) = oset.new("oset13", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [A(1), B(2, 3), C])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, A(1))
//   |> should.equal(Ok(A(1)))
//   oset.lookup(table, B(2, 3))
//   |> should.equal(Ok(B(2, 3)))
//   oset.lookup(table, C)
//   |> should.equal(Ok(C))
// }
//
// pub fn oset_delete_key_test() {
//   let assert Ok(table) = oset.new("oset14", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, "Bye")
//   |> should.equal(Ok(#("Bye", "World")))
//   oset.delete_key(table, "Bye")
//   oset.lookup(table, "Bye")
//   |> should.equal(Error(bravo.Empty))
//   oset.lookup(table, "Hello")
//   |> should.equal(Ok(#("Hello", "World")))
// }
//
// pub fn oset_delete_all_objects_test() {
//   let assert Ok(table) = oset.new("oset15", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.equal(Ok(Nil))
//   oset.delete_all_objects(table)
//   oset.lookup(table, "Hello")
//   |> should.equal(Error(bravo.Empty))
//   oset.lookup(table, "Bye")
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_delete_object_test() {
//   let assert Ok(table) = oset.new("oset16", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.equal(Ok(Nil))
//   oset.delete_object(table, #("Bye", "World"))
//   oset.lookup(table, "Hello")
//   |> should.equal(Ok(#("Hello", "World")))
//   oset.lookup(table, "Bye")
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_tab2file_test() {
//   let assert Ok(table) = oset.new("oset17", 2, bravo.Public)
//   oset.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   oset.tab2file(table, "oset17", True, True, True)
//   |> should.equal(Ok(Nil))
//   oset.delete(table)
//   let assert Ok(new_table) =
//     oset.file2tab(
//       "oset17",
//       True,
//       dynamic.tuple2(dynamic.string, dynamic.string),
//     )
//   oset.lookup(new_table, "World")
//   |> should.equal(Ok(#("Hello", "World")))
//   oset.delete(new_table)
//   oset.file2tab("oset17", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("oset17")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn oset_tab2list_test() {
//   let assert Ok(table) = oset.new("oset18", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.equal(Ok(Nil))
//   let objects = oset.tab2list(table)
//   list.contains(objects, #("Hello", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "Bye"))
//   |> should.equal(False)
// }
//
// pub fn oset_tab2list_orderedness_test() {
//   let assert Ok(table) = oset.new("oset19", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
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
//   oset.tab2list(table)
//   |> should.equal([
//     #("A"),
//     #("B"),
//     #("C"),
//     #("DA"),
//     #("Da"),
//     #("Db"),
//     #("F"),
//     #("Q"),
//     #("R"),
//     #("S"),
//     #("Z"),
//     #("a"),
//   ])
// }
//
// pub fn oset_empty_insert_test() {
//   let assert Ok(table) = oset.new("oset20", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [])
//   |> should.equal(Error(bravo.NothingToInsert))
// }
//
// pub fn oset_dynamic_test() {
//   let assert Ok(table) = oset.new("oset21", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
//     dynamic.from(#("Hello", "World")),
//     dynamic.from(#(1, 2, 3)),
//   ])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, "Hello")
//   |> should.equal(Ok(dynamic.from(#("Hello", "World"))))
//   oset.lookup(table, 1)
//   |> should.equal(Ok(dynamic.from(#(1, 2, 3))))
// }
//
// pub fn oset_insert_new_test() {
//   let assert Ok(table) = oset.new("oset22", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert_new(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   oset.insert_new(table, [#(1, 3), #(2, 4)])
//   |> should.equal(Error(bravo.KeyAlreadyPresent))
//   oset.lookup(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   oset.lookup(table, 2)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_take_test() {
//   let assert Ok(table) = oset.new("oset23", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   oset.lookup(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   oset.take(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   oset.take(table, 1)
//   |> should.equal(Error(bravo.Empty))
//   oset.lookup(table, 1)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn oset_member_test() {
//   let assert Ok(table) = oset.new("oset24", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   oset.member(table, 1)
//   |> should.equal(True)
//   oset.member(table, 2)
//   |> should.equal(False)
//   oset.delete_key(table, 1)
//   oset.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn oset_singleton_member_test() {
//   let assert Ok(table) = oset.new("oset25", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [1, 3])
//   |> should.equal(Ok(Nil))
//   oset.member(table, 1)
//   |> should.equal(True)
//   oset.member(table, 2)
//   |> should.equal(False)
//   oset.delete_key(table, 1)
//   oset.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn oset_tab2file_singleton_test() {
//   let assert Ok(table) = oset.new("oset26", 1, bravo.Public)
//   oset.insert(table, ["Hello"])
//   |> should.equal(Ok(Nil))
//   oset.tab2file(table, "oset26", True, True, True)
//   |> should.equal(Ok(Nil))
//   oset.delete(table)
//   let assert Ok(new_table) = oset.file2tab("oset26", True, dynamic.string)
//   oset.lookup(new_table, "Hello")
//   |> should.equal(Ok("Hello"))
//   oset.delete(new_table)
//   oset.file2tab("oset26", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//
//   let assert Ok(newer_table) = oset.new("oset26", 1, bravo.Public)
//   oset.insert(newer_table, [#("Hello")])
//   |> should.equal(Ok(Nil))
//   oset.tab2file(newer_table, "oset26", True, True, True)
//   |> should.equal(Ok(Nil))
//   oset.delete(newer_table)
//   let assert Ok(newest_table) = oset.file2tab("oset26", True, dynamic.string)
//   oset.lookup(newest_table, "Hello")
//   |> should.equal(Ok("Hello"))
//   oset.delete(newest_table)
//
//   simplifile.delete("oset26")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn oset_tab2file_singleton_record_test() {
//   let assert Ok(table) = oset.new("oset27", 1, bravo.Public)
//   oset.insert(table, [Ok("Hello"), Error("World")])
//   |> should.equal(Ok(Nil))
//   oset.tab2file(table, "oset27", True, True, True)
//   |> should.equal(Ok(Nil))
//   oset.delete(table)
//   let assert Ok(new_table) =
//     oset.file2tab(
//       "oset27",
//       True,
//       dynamic.result(dynamic.string, dynamic.string),
//     )
//   oset.lookup(new_table, Ok("Hello"))
//   |> should.equal(Ok(Ok("Hello")))
//   oset.lookup(new_table, Error("World"))
//   |> should.equal(Ok(Error("World")))
//   oset.delete(new_table)
//   oset.file2tab("oset27", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("oset27")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn oset_fn_test() {
//   let assert Ok(table) = oset.new("oset28", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
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
//   let assert Ok(a) = table |> oset.first
//   let assert Ok(b) = table |> oset.next(a)
//   let assert Ok(c) = table |> oset.next(b)
//   let assert Ok(d) = table |> oset.next(c)
//   let assert Ok(e) = table |> oset.next(d)
//   let assert Ok(f) = table |> oset.next(e)
//   let assert Ok(g) = table |> oset.next(f)
//   let assert Ok(h) = table |> oset.next(g)
//   let assert Ok(i) = table |> oset.next(h)
//   let assert Ok(j) = table |> oset.next(i)
//   let assert Ok(k) = table |> oset.next(j)
//   let assert Ok(l) = table |> oset.next(k)
//   oset.lookup(table, a) |> should.equal(Ok(#("A")))
//   oset.lookup(table, b) |> should.equal(Ok(#("B")))
//   oset.lookup(table, c) |> should.equal(Ok(#("C")))
//   oset.lookup(table, d) |> should.equal(Ok(#("DA")))
//   oset.lookup(table, e) |> should.equal(Ok(#("Da")))
//   oset.lookup(table, f) |> should.equal(Ok(#("Db")))
//   oset.lookup(table, g) |> should.equal(Ok(#("F")))
//   oset.lookup(table, h) |> should.equal(Ok(#("Q")))
//   oset.lookup(table, i) |> should.equal(Ok(#("R")))
//   oset.lookup(table, j) |> should.equal(Ok(#("S")))
//   oset.lookup(table, k) |> should.equal(Ok(#("Z")))
//   oset.lookup(table, l) |> should.equal(Ok(#("a")))
//   table |> oset.next(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// pub fn oset_lp_test() {
//   let assert Ok(table) = oset.new("oset28", 1, bravo.Public)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [
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
//   let assert Ok(a) = table |> oset.last
//   let assert Ok(b) = table |> oset.prev(a)
//   let assert Ok(c) = table |> oset.prev(b)
//   let assert Ok(d) = table |> oset.prev(c)
//   let assert Ok(e) = table |> oset.prev(d)
//   let assert Ok(f) = table |> oset.prev(e)
//   let assert Ok(g) = table |> oset.prev(f)
//   let assert Ok(h) = table |> oset.prev(g)
//   let assert Ok(i) = table |> oset.prev(h)
//   let assert Ok(j) = table |> oset.prev(i)
//   let assert Ok(k) = table |> oset.prev(j)
//   let assert Ok(l) = table |> oset.prev(k)
//   oset.lookup(table, a) |> should.equal(Ok(#("a")))
//   oset.lookup(table, b) |> should.equal(Ok(#("Z")))
//   oset.lookup(table, c) |> should.equal(Ok(#("S")))
//   oset.lookup(table, d) |> should.equal(Ok(#("R")))
//   oset.lookup(table, e) |> should.equal(Ok(#("Q")))
//   oset.lookup(table, f) |> should.equal(Ok(#("F")))
//   oset.lookup(table, g) |> should.equal(Ok(#("Db")))
//   oset.lookup(table, h) |> should.equal(Ok(#("Da")))
//   oset.lookup(table, i) |> should.equal(Ok(#("DA")))
//   oset.lookup(table, j) |> should.equal(Ok(#("C")))
//   oset.lookup(table, k) |> should.equal(Ok(#("B")))
//   oset.lookup(table, l) |> should.equal(Ok(#("A")))
//   table |> oset.prev(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// // TODO: Replace the internal binding calls here with actual functions
// pub fn oset_async_access_test() {
//   let assert Ok(table) = oset.new("oset30", 1, bravo.Private)
//   use <- defer(fn() { oset.delete(table) |> should.equal(True) })
//   oset.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   let assert Ok(ref) =
//     "oset30"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   let task = {
//     use <- task.async
//     let assert Ok(ref) =
//       "oset30"
//       |> atom.create_from_string
//       |> bindings.try_whereis
//     bindings.try_lookup(ref, "Hello")
//     |> should.equal(Error(bravo.AccessDenied))
//   }
//   task.await_forever(task)
// }
//
// pub fn oset_async_protected_test() {
//   let assert Ok(actor) = {
//     use _, _ <- actor.start(option.None)
//     let assert Ok(table) = oset.new("oset30a", 1, bravo.Protected)
//     oset.insert(table, [#("Goodbye", "World")])
//     |> should.equal(Ok(Nil))
//     actor.Continue(option.Some(table), option.None)
//   }
//   actor.send(actor, Nil)
//   process.sleep(100)
//   let assert Ok(ref) =
//     "oset30a"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Goodbye")
//   |> should.equal(Ok([#("Goodbye", "World")]))
//   bindings.try_insert(ref, 1, [#("Hello", "Again")])
//   |> should.equal(Error(bravo.AccessDenied))
// }
//
// pub fn oset_recreation_test() {
//   let assert Ok(table) = oset.new("oset31", 1, bravo.Private)
//   oset.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   oset.delete(table)
//   let assert Ok(_table2) = oset.new("oset31", 1, bravo.Private)
//   oset.insert(table, [#("Hello", "World")])
//   |> should.equal(Error(bravo.TableDoesNotExist))
// }
