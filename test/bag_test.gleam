import bravo
import bravo/bag
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/otp/task
import gleeunit/should
import shellout
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn bag_insert_lookup_delete_test() {
  let assert Ok(table) = bag.new("bag1", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert(table, "a", 1)
  |> should.be_ok
  bag.lookup(table, "a")
  |> should.equal(Ok([1]))
  bag.lookup(table, "b")
  |> should.equal(Error(bravo.Empty))
  bag.insert_list(table, [#("b", 2), #("c", 3)])
  |> should.be_ok
  bag.lookup(table, "b")
  |> should.equal(Ok([2]))
  bag.lookup(table, "c")
  |> should.equal(Ok([3]))
}

pub fn bag_dupe_test() {
  let assert Ok(table) = bag.new("bag2", bravo.Public)
  bag.new("bag2", bravo.Public)
  |> should.equal(Error(bravo.TableAlreadyExists))
  bag.delete(table)
  |> should.be_ok
  let assert Ok(table) = bag.new("bag2", bravo.Public)
  bag.delete(table)
}

pub fn bag_multi_insert_test() {
  let assert Ok(table) = bag.new("bag3", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert(table, 100, 200)
  |> should.be_ok
  bag.lookup(table, 100)
  |> should.equal(Ok([200]))
  bag.insert(table, 100, 300)
  |> should.be_ok
  bag.lookup(table, 100)
  |> should.equal(Ok([200, 300]))
  bag.insert(table, 100, 300)
  |> should.be_ok
  bag.lookup(table, 100)
  |> should.equal(Ok([200, 300]))
}

pub fn bag_large_multitype_test() {
  let assert Ok(table) = bag.new("bag4", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert(table, 0, #(
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
  bag.lookup(table, 0)
  |> should.equal(
    Ok([
      #(
        "String",
        5,
        10.0,
        [15, 20],
        #(25, 30),
        dict.from_list([#(35, 40)]),
        Ok(45),
        Ok(50),
      ),
    ]),
  )
}

pub fn bag_delete_key_test() {
  let assert Ok(table) = bag.new("bag5", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  bag.lookup(table, "Bye")
  |> should.equal(Ok(["World"]))
  bag.delete_key(table, "Bye")
  |> should.be_ok
  bag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
  bag.lookup(table, "Hello")
  |> should.equal(Ok(["World"]))
}

pub fn bag_delete_all_objects_test() {
  let assert Ok(table) = bag.new("bag6", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  bag.delete_all_objects(table)
  |> should.be_ok
  bag.lookup(table, "Hello")
  |> should.equal(Error(bravo.Empty))
  bag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn bag_delete_object_test() {
  let assert Ok(table) = bag.new("bag7", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  bag.delete_object(table, "Bye", "World")
  |> should.be_ok
  bag.lookup(table, "Hello")
  |> should.equal(Ok(["World"]))
  bag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn bag_delete_table_test() {
  let assert Ok(table) = bag.new("bag8", bravo.Public)
  bag.delete(table)
  |> should.be_ok
  bag.insert(table, "Hello", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
}

pub fn bag_tab2file_test() {
  let assert Ok(table) = bag.new("bag9", bravo.Public)
  bag.insert(table, "Hello", "World")
  |> should.be_ok
  bag.tab2file(table, "bag9", True, True, True)
  |> should.be_ok
  bag.delete(table)
  |> should.be_ok
  shellout.command("mkdir", ["no_access", "-p", "-m", "555"], ".", [])
  |> should.be_ok
  bag.tab2file(table, "no_access/bag9", True, True, True)
  |> should.equal(Error(bravo.NoFilePermissions))
}

pub fn bag_file2tab_test() {
  let assert Ok(new_table) =
    bag.file2tab("bag9", True, dynamic.string, dynamic.string)
  bag.lookup(new_table, "Hello")
  |> should.equal(Ok(["World"]))
  bag.delete(new_table)
  |> should.be_ok
  bag.file2tab("bag9", True, dynamic.int, dynamic.int)
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("bag9")
  |> should.be_ok
  bag.file2tab("no_access/bag9", True, dynamic.string, dynamic.string)
  |> should.equal(Error(bravo.FileDoesNotExist))
}

pub fn bag_access_test() {
  let assert Ok(table) = bag.new("bag10", bravo.Protected)
  bag.insert(table, "Hello", "World")
  |> should.be_ok
  {
    use <- task.async
    bag.lookup(table, "Hello")
    |> should.equal(Ok(["World"]))
    bag.insert(table, "Goodbye", "World")
    |> should.equal(Error(bravo.AccessDenied))
  }
  |> task.await_forever
}

pub fn bag_tab2list_orderedness_test() {
  let assert Ok(table) = bag.new("bag11", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  let control = [
    #("A", "B"),
    #("Q", "S"),
    #("C", "F"),
    #("R", "Da"),
    #("Z", "DA"),
    #("B", "Db"),
    #("S", "a"),
    #("F", "A"),
    #("Da", "Q"),
    #("DA", "C"),
    #("Db", "R"),
    #("a", "Z"),
  ]
  bag.insert_list(table, control)
  |> should.be_ok
  let assert Ok(list) = bag.tab2list(table)
  list
  |> should.not_equal([
    #("A", "B"),
    #("B", "Db"),
    #("C", "F"),
    #("DA", "C"),
    #("Da", "Q"),
    #("Db", "R"),
    #("F", "A"),
    #("Q", "S"),
    #("R", "Da"),
    #("S", "a"),
    #("Z", "DA"),
    #("a", "Z"),
  ])
  list
  |> list.length
  |> should.equal(12)
  use elem <- list.each(control)
  list.contains(list, elem)
  |> should.be_true
}

pub fn bag_insert_new_test() {
  let assert Ok(table) = bag.new("bag12", bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.be_ok })
  bag.insert_new(table, 1, 2)
  |> should.be_ok
  bag.insert_new_list(table, [#(1, 3), #(2, 4)])
  |> should.equal(Error(bravo.KeyAlreadyPresent))
  bag.lookup(table, 1)
  |> should.equal(Ok([2]))
  bag.lookup(table, 2)
  |> should.equal(Error(bravo.Empty))
}
//
// pub fn bag_insert_new_test() {
//   let assert Ok(table) = bag.new("bag22", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert_new(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   bag.insert_new(table, [#(1, 3), #(2, 4)])
//   |> should.equal(Error(bravo.KeyAlreadyPresent))
//   bag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   bag.lookup(table, 2)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn bag_take_test() {
//   let assert Ok(table) = bag.new("bag23", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   bag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   bag.take(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   bag.take(table, 1)
//   |> should.equal(Error(bravo.Empty))
//   bag.lookup(table, 1)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn bag_member_test() {
//   let assert Ok(table) = bag.new("bag24", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   bag.member(table, 1)
//   |> should.equal(True)
//   bag.member(table, 2)
//   |> should.equal(False)
//   bag.delete_key(table, 1)
//   bag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn bag_singleton_member_test() {
//   let assert Ok(table) = bag.new("bag25", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert(table, [1, 3])
//   |> should.equal(Ok(Nil))
//   bag.member(table, 1)
//   |> should.equal(True)
//   bag.member(table, 2)
//   |> should.equal(False)
//   bag.delete_key(table, 1)
//   bag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn bag_tab2file_singleton_test() {
//   let assert Ok(table) = bag.new("bag26", 1, bravo.Public)
//   bag.insert(table, ["Hello"])
//   |> should.equal(Ok(Nil))
//   bag.tab2file(table, "bag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   bag.delete(table)
//   let assert Ok(new_table) = bag.file2tab("bag26", True, dynamic.string)
//   bag.lookup(new_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   bag.delete(new_table)
//   bag.file2tab("bag26", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//
//   let assert Ok(newer_table) = bag.new("bag26", 1, bravo.Public)
//   bag.insert(newer_table, [#("Hello")])
//   |> should.equal(Ok(Nil))
//   bag.tab2file(newer_table, "bag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   bag.delete(newer_table)
//   let assert Ok(newest_table) = bag.file2tab("bag26", True, dynamic.string)
//   bag.lookup(newest_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   bag.delete(newest_table)
//
//   simplifile.delete("bag26")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn bag_tab2file_singleton_record_test() {
//   let assert Ok(table) = bag.new("bag27", 1, bravo.Public)
//   bag.insert(table, [Ok("Hello"), Error("World")])
//   |> should.equal(Ok(Nil))
//   bag.tab2file(table, "bag27", True, True, True)
//   |> should.equal(Ok(Nil))
//   bag.delete(table)
//   let assert Ok(new_table) =
//     bag.file2tab("bag27", True, dynamic.result(dynamic.string, dynamic.string))
//   bag.lookup(new_table, Ok("Hello"))
//   |> should.equal(Ok([Ok("Hello")]))
//   bag.lookup(new_table, Error("World"))
//   |> should.equal(Ok([Error("World")]))
//   bag.delete(new_table)
//   bag.file2tab("bag27", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("bag27")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn bag_fn_test() {
//   let assert Ok(table) = bag.new("bag28", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   let dataset = ["A", "Q", "C", "R", "Z", "B", "S", "F", "Da", "DA", "Db", "a"]
//   bag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> bag.first
//   let assert Ok(a) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(b) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(c) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(d) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(e) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(f) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(g) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(h) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(i) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(j) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(k) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.next(key)
//   let assert Ok(l) = bag.lookup(table, key)
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
//   table |> bag.next(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// pub fn bag_lp_test() {
//   let assert Ok(table) = bag.new("bag29", 1, bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   let dataset = ["A", "Q", "C", "R", "Z", "B", "S", "F", "Da", "DA", "Db", "a"]
//   bag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> bag.last
//   let assert Ok(a) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(b) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(c) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(d) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(e) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(f) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(g) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(h) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(i) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(j) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(k) = bag.lookup(table, key)
//   let assert Ok(key) = table |> bag.prev(key)
//   let assert Ok(l) = bag.lookup(table, key)
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
//   table |> bag.prev(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// // TODO: Replace the internal binding calls here with actual functions
// pub fn bag_async_access_test() {
//   let assert Ok(table) = bag.new("bag30", 1, bravo.Private)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   let assert Ok(ref) =
//     "bag30"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   let task = {
//     use <- task.async
//     let assert Ok(ref) =
//       "bag30"
//       |> atom.create_from_string
//       |> bindings.try_whereis
//     bindings.try_lookup(ref, "Hello")
//     |> should.equal(Error(bravo.AccessDenied))
//   }
//   task.await_forever(task)
// }
//
// pub fn bag_async_protected_test() {
//   let assert Ok(actor) = {
//     use _, _ <- actor.start(option.None)
//     let assert Ok(table) = bag.new("bag30a", 1, bravo.Protected)
//     bag.insert(table, [#("Goodbye", "World")])
//     |> should.equal(Ok(Nil))
//     actor.Continue(option.Some(table), option.None)
//   }
//   actor.send(actor, Nil)
//   process.sleep(100)
//   let assert Ok(ref) =
//     "bag30a"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Goodbye")
//   |> should.equal(Ok([#("Goodbye", "World")]))
//   bindings.try_insert(ref, 1, [#("Hello", "Again")])
//   |> should.equal(Error(bravo.AccessDenied))
// }
//
// pub fn bag_recreation_test() {
//   let assert Ok(table) = bag.new("bag31", 1, bravo.Private)
//   bag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   bag.delete(table)
//   let assert Ok(_table2) = bag.new("bag31", 1, bravo.Private)
//   bag.insert(table, [#("Hello", "World")])
//   |> should.equal(Error(bravo.TableDoesNotExist))
// }
