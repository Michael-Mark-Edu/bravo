import bravo
import bravo/uset
import gleam/dict
import gleam/dynamic
import gleeunit/should
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn uset_insert_lookup_delete_test() {
  let assert Ok(table) = uset.new("uset1", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, "a", 1)
  |> should.be_ok
  uset.lookup(table, "a")
  |> should.equal(Ok(1))
  uset.lookup(table, "b")
  |> should.equal(Error(bravo.Empty))
  uset.insert_list(table, [#("b", 2), #("c", 3)])
  |> should.be_ok
  uset.lookup(table, "b")
  |> should.equal(Ok(2))
  uset.lookup(table, "c")
  |> should.equal(Ok(3))
}

pub fn uset_large_test() {
  let assert Ok(table) = uset.new("uset2", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, #(1, 2, 3, 4, 5), [6, 7, 8, 9, 0])
  |> should.be_ok
  uset.lookup(table, #(1, 2, 3, 4, 5))
  |> should.equal(Ok([6, 7, 8, 9, 0]))
}

pub fn uset_dupe_test() {
  let assert Ok(table) = uset.new("uset3", bravo.Public)
  uset.new("uset3", bravo.Public)
  |> should.equal(Error(bravo.TableAlreadyExists))
  uset.delete(table)
  let assert Ok(table) = uset.new("uset3", bravo.Public)
  uset.delete(table)
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) = uset.new("uset4", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, 100, 200)
  |> should.be_ok
  uset.lookup(table, 100)
  |> should.equal(Ok(200))
  uset.insert(table, 100, 300)
  |> should.be_ok
  uset.lookup(table, 100)
  |> should.equal(Ok(300))
}

pub fn uset_large_multitype_test() {
  let assert Ok(table) = uset.new("uset5", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, 0, #(
    "String",
    5,
    10.0,
    [15, 20],
    #(25, 30),
    dict.from_list([#(35, 40)]),
    Ok(45),
    Ok(50),
  ))
  |> should.be_ok
  uset.lookup(table, 0)
  |> should.equal(
    Ok(#(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Ok(50),
    )),
  )
}

pub fn uset_delete_key_test() {
  let assert Ok(table) = uset.new("uset6", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.lookup(table, "Bye")
  |> should.equal(Ok("World"))
  uset.delete_key(table, "Bye")
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
}

pub fn uset_delete_all_objects_test() {
  let assert Ok(table) = uset.new("uset7", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.delete_all_objects(table)
  uset.lookup(table, "Hello")
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn uset_delete_object_test() {
  let assert Ok(table) = uset.new("uset8", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.delete_object(table, #("Bye", "World"))
  uset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn uset_tab2file_test() {
  let assert Ok(table) = uset.new("uset9", bravo.Public)
  uset.insert(table, "Hello", "World")
  |> should.be_ok
  uset.tab2file(table, "uset9", True, True, True)
  |> should.be_ok
  uset.delete(table)
}

pub fn uset_file2tab_test() {
  let assert Ok(new_table) =
    uset.file2tab("uset9", True, dynamic.string, dynamic.string)
  uset.lookup(new_table, "Hello")
  |> should.equal(Ok("World"))
  uset.delete(new_table)
  uset.file2tab("uset9", True, dynamic.int, dynamic.int)
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("uset9")
  |> should.be_ok
}
//
// pub fn uset_tab2list_test() {
//   let assert Ok(table) = uset.new("uset18", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [#("Hello", "World"), #("Bye", "World")])
//   |> should.be_ok
//   let objects = uset.tab2list(table)
//   list.contains(objects, #("Hello", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "World"))
//   |> should.equal(True)
//   list.contains(objects, #("Bye", "Bye"))
//   |> should.equal(False)
// }
//
// pub fn uset_tab2list_orderedness_test() {
//   let assert Ok(table) = uset.new("uset19", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [
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
//   |> should.be_ok
//   uset.tab2list(table)
//   |> should.not_equal([
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
// pub fn uset_empty_insert_test() {
//   let assert Ok(table) = uset.new("uset20", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [])
//   |> should.equal(Error(bravo.NothingToInsert))
// }
//
// pub fn uset_dynamic_test() {
//   let assert Ok(table) = uset.new("uset21", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [
//     dynamic.from(#("Hello", "World")),
//     dynamic.from(#(1, 2, 3)),
//   ])
//   |> should.be_ok
//   uset.lookup(table, "Hello")
//   |> should.equal(Ok(dynamic.from(#("Hello", "World"))))
//   uset.lookup(table, 1)
//   |> should.equal(Ok(dynamic.from(#(1, 2, 3))))
// }
//
// pub fn uset_insert_new_test() {
//   let assert Ok(table) = uset.new("uset22", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert_new(table, [#(1, 2), #(3, 4)])
//   |> should.be_ok
//   uset.insert_new(table, [#(1, 3), #(2, 4)])
//   |> should.equal(Error(bravo.KeyAlreadyPresent))
//   uset.lookup(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   uset.lookup(table, 2)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn uset_take_test() {
//   let assert Ok(table) = uset.new("uset23", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [#(1, 2), #(3, 4)])
//   |> should.be_ok
//   uset.lookup(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   uset.take(table, 1)
//   |> should.equal(Ok(#(1, 2)))
//   uset.take(table, 1)
//   |> should.equal(Error(bravo.Empty))
//   uset.lookup(table, 1)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn uset_member_test() {
//   let assert Ok(table) = uset.new("uset24", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [#(1, 2), #(3, 4)])
//   |> should.be_ok
//   uset.member(table, 1)
//   |> should.equal(True)
//   uset.member(table, 2)
//   |> should.equal(False)
//   uset.delete_key(table, 1)
//   uset.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn uset_singleton_member_test() {
//   let assert Ok(table) = uset.new("uset25", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [1, 3])
//   |> should.be_ok
//   uset.member(table, 1)
//   |> should.equal(True)
//   uset.member(table, 2)
//   |> should.equal(False)
//   uset.delete_key(table, 1)
//   uset.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn uset_tab2file_singleton_test() {
//   let assert Ok(table) = uset.new("uset26", 1, bravo.Public)
//   uset.insert(table, ["Hello"])
//   |> should.be_ok
//   uset.tab2file(table, "uset26", True, True, True)
//   |> should.be_ok
//   uset.delete(table)
//   let assert Ok(new_table) = uset.file2tab("uset26", True, dynamic.string)
//   uset.lookup(new_table, "Hello")
//   |> should.equal(Ok("Hello"))
//   uset.delete(new_table)
//   uset.file2tab("uset26", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//
//   let assert Ok(newer_table) = uset.new("uset26", 1, bravo.Public)
//   uset.insert(newer_table, [#("Hello")])
//   |> should.be_ok
//   uset.tab2file(newer_table, "uset26", True, True, True)
//   |> should.be_ok
//   uset.delete(newer_table)
//   let assert Ok(newest_table) = uset.file2tab("uset26", True, dynamic.string)
//   uset.lookup(newest_table, "Hello")
//   |> should.equal(Ok("Hello"))
//   uset.delete(newest_table)
//
//   simplifile.delete("uset26")
//   |> should.be_ok
// }
//
// pub fn uset_tab2file_singleton_record_test() {
//   let assert Ok(table) = uset.new("uset27", 1, bravo.Public)
//   uset.insert(table, [Ok("Hello"), Error("World")])
//   |> should.be_ok
//   uset.tab2file(table, "uset27", True, True, True)
//   |> should.be_ok
//   uset.delete(table)
//   let assert Ok(new_table) =
//     uset.file2tab(
//       "uset27",
//       True,
//       dynamic.result(dynamic.string, dynamic.string),
//     )
//   uset.lookup(new_table, Ok("Hello"))
//   |> should.equal(Ok(Ok("Hello")))
//   uset.lookup(new_table, Error("World"))
//   |> should.equal(Ok(Error("World")))
//   uset.delete(new_table)
//   uset.file2tab("uset27", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("uset27")
//   |> should.be_ok
// }
//
// pub fn uset_fn_test() {
//   let assert Ok(table) = uset.new("uset28", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   let dataset = ["A", "Q", "C", "R", "Z", "B", "S", "F", "Da", "DA", "Db", "a"]
//   uset.insert(table, dataset)
//   |> should.be_ok
//   let assert Ok(key) = table |> uset.first
//   let assert Ok(a) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(b) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(c) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(d) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(e) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(f) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(g) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(h) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(i) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(j) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(k) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.next(key)
//   let assert Ok(l) = uset.lookup(table, key)
//   let list = []
//   let list = list.append([a], list)
//   let list = list.append([b], list)
//   let list = list.append([c], list)
//   let list = list.append([d], list)
//   let list = list.append([e], list)
//   let list = list.append([f], list)
//   let list = list.append([g], list)
//   let list = list.append([h], list)
//   let list = list.append([i], list)
//   let list = list.append([j], list)
//   let list = list.append([k], list)
//   let list = list.append([l], list)
//   list.map(dataset, fn(elem) {
//     list.contains(list, elem)
//     |> should.equal(True)
//   })
//   table |> uset.next(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// pub fn uset_lp_test() {
//   let assert Ok(table) = uset.new("uset29", 1, bravo.Public)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   let dataset = ["A", "Q", "C", "R", "Z", "B", "S", "F", "Da", "DA", "Db", "a"]
//   uset.insert(table, dataset)
//   |> should.be_ok
//   let assert Ok(key) = table |> uset.last
//   let assert Ok(a) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(b) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(c) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(d) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(e) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(f) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(g) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(h) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(i) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(j) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(k) = uset.lookup(table, key)
//   let assert Ok(key) = table |> uset.prev(key)
//   let assert Ok(l) = uset.lookup(table, key)
//   let list = []
//   let list = list.append([a], list)
//   let list = list.append([b], list)
//   let list = list.append([c], list)
//   let list = list.append([d], list)
//   let list = list.append([e], list)
//   let list = list.append([f], list)
//   let list = list.append([g], list)
//   let list = list.append([h], list)
//   let list = list.append([i], list)
//   let list = list.append([j], list)
//   let list = list.append([k], list)
//   let list = list.append([l], list)
//   list.map(dataset, fn(elem) {
//     list.contains(list, elem)
//     |> should.equal(True)
//   })
//   table |> uset.prev(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// // TODO: Replace the internal binding calls here with actual functions
// pub fn uset_async_access_test() {
//   let assert Ok(table) = uset.new("uset30", 1, bravo.Private)
//   use <- defer(fn() { uset.delete(table) |> should.equal(True) })
//   uset.insert(table, [#("Hello", "World")])
//   |> should.be_ok
//   let assert Ok(ref) =
//     "uset30"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   let task = {
//     use <- task.async
//     let assert Ok(ref) =
//       "uset30"
//       |> atom.create_from_string
//       |> bindings.try_whereis
//     bindings.try_lookup(ref, "Hello")
//     |> should.equal(Error(bravo.AccessDenied))
//   }
//   task.await_forever(task)
// }
//
// pub fn uset_async_protected_test() {
//   let assert Ok(actor) = {
//     use _, _ <- actor.start(option.None)
//     let assert Ok(table) = uset.new("uset30a", 1, bravo.Protected)
//     uset.insert(table, [#("Goodbye", "World")])
//     |> should.be_ok
//     actor.Continue(option.Some(table), option.None)
//   }
//   actor.send(actor, Nil)
//   process.sleep(100)
//   let assert Ok(ref) =
//     "uset30a"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Goodbye")
//   |> should.equal(Ok([#("Goodbye", "World")]))
//   bindings.try_insert(ref, 1, [#("Hello", "Again")])
//   |> should.equal(Error(bravo.AccessDenied))
// }
//
// pub fn uset_recreation_test() {
//   let assert Ok(table) = uset.new("uset31", 1, bravo.Public)
//   uset.insert(table, [#("Hello", "World")])
//   |> should.be_ok
//   uset.delete(table)
//   let assert Ok(_table2) = uset.new("uset31", 1, bravo.Public)
//   uset.insert(table, [#("Hello", "World")])
//   |> should.equal(Error(bravo.TableDoesNotExist))
// }
