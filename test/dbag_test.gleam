import bravo
import bravo/dbag
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

pub fn dbag_insert_lookup_delete_test() {
  let assert Ok(table) = dbag.new("dbag1", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert(table, "a", 1)
  |> should.be_ok
  dbag.lookup(table, "a")
  |> should.equal(Ok([1]))
  dbag.lookup(table, "b")
  |> should.equal(Error(bravo.Empty))
  dbag.insert_list(table, [#("b", 2), #("c", 3)])
  |> should.be_ok
  dbag.lookup(table, "b")
  |> should.equal(Ok([2]))
  dbag.lookup(table, "c")
  |> should.equal(Ok([3]))
}

pub fn dbag_dupe_test() {
  let assert Ok(table) = dbag.new("dbag2", bravo.Public)
  dbag.new("dbag2", bravo.Public)
  |> should.equal(Error(bravo.TableAlreadyExists))
  dbag.delete(table)
  |> should.be_ok
  let assert Ok(table) = dbag.new("dbag2", bravo.Public)
  dbag.delete(table)
}

pub fn dbag_multi_insert_test() {
  let assert Ok(table) = dbag.new("dbag3", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert(table, 100, 200)
  |> should.be_ok
  dbag.lookup(table, 100)
  |> should.equal(Ok([200]))
  dbag.insert(table, 100, 300)
  |> should.be_ok
  dbag.lookup(table, 100)
  |> should.equal(Ok([200, 300]))
  dbag.insert(table, 100, 300)
  |> should.be_ok
  dbag.lookup(table, 100)
  |> should.equal(Ok([200, 300, 300]))
}

pub fn dbag_large_multitype_test() {
  let assert Ok(table) = dbag.new("dbag4", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert(table, 0, #(
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
  dbag.lookup(table, 0)
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

pub fn dbag_delete_key_test() {
  let assert Ok(table) = dbag.new("dbag5", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  dbag.lookup(table, "Bye")
  |> should.equal(Ok(["World"]))
  dbag.delete_key(table, "Bye")
  |> should.be_ok
  dbag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
  dbag.lookup(table, "Hello")
  |> should.equal(Ok(["World"]))
}

pub fn dbag_delete_all_objects_test() {
  let assert Ok(table) = dbag.new("dbag6", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  dbag.delete_all_objects(table)
  |> should.be_ok
  dbag.lookup(table, "Hello")
  |> should.equal(Error(bravo.Empty))
  dbag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn dbag_delete_object_test() {
  let assert Ok(table) = dbag.new("dbag7", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  dbag.delete_object(table, "Bye", "World")
  |> should.be_ok
  dbag.lookup(table, "Hello")
  |> should.equal(Ok(["World"]))
  dbag.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn dbag_delete_table_test() {
  let assert Ok(table) = dbag.new("dbag8", bravo.Public)
  dbag.delete(table)
  |> should.be_ok
  dbag.insert(table, "Hello", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
}

pub fn dbag_tab2file_test() {
  let assert Ok(table) = dbag.new("dbag9", bravo.Public)
  dbag.insert(table, "Hello", "World")
  |> should.be_ok
  dbag.tab2file(table, "dbag9", True, True, True)
  |> should.be_ok
  dbag.delete(table)
  |> should.be_ok
  shellout.command("mkdir", ["no_access", "-p", "-m", "555"], ".", [])
  |> should.be_ok
  dbag.tab2file(table, "no_access/dbag9", True, True, True)
  |> should.equal(Error(bravo.NoFilePermissions))
}

pub fn dbag_file2tab_test() {
  let assert Ok(new_table) =
    dbag.file2tab("dbag9", True, dynamic.string, dynamic.string)
  dbag.lookup(new_table, "Hello")
  |> should.equal(Ok(["World"]))
  dbag.delete(new_table)
  |> should.be_ok
  dbag.file2tab("dbag9", True, dynamic.int, dynamic.int)
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("dbag9")
  |> should.be_ok
  dbag.file2tab("no_access/dbag9", True, dynamic.string, dynamic.string)
  |> should.equal(Error(bravo.FileDoesNotExist))
}

pub fn dbag_access_test() {
  let assert Ok(table) = dbag.new("dbag10", bravo.Protected)
  dbag.insert(table, "Hello", "World")
  |> should.be_ok
  {
    use <- task.async
    dbag.lookup(table, "Hello")
    |> should.equal(Ok(["World"]))
    dbag.insert(table, "Goodbye", "World")
    |> should.equal(Error(bravo.AccessDenied))
  }
  |> task.await_forever
}

pub fn dbag_tab2list_orderedness_test() {
  let assert Ok(table) = dbag.new("dbag11", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
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
  dbag.insert_list(table, control)
  |> should.be_ok
  let assert Ok(list) = dbag.tab2list(table)
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

pub fn dbag_insert_new_test() {
  let assert Ok(table) = dbag.new("dbag12", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_new(table, 1, 2)
  |> should.be_ok
  dbag.insert_new_list(table, [#(1, 3), #(2, 4)])
  |> should.equal(Error(bravo.KeyAlreadyPresent))
  dbag.lookup(table, 1)
  |> should.equal(Ok([2]))
  dbag.lookup(table, 2)
  |> should.equal(Error(bravo.Empty))
}

pub fn dbag_take_test() {
  let assert Ok(table) = dbag.new("dbag13", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_list(table, [#(1, 2), #(1, 3), #(1, 3), #(3, 4)])
  |> should.be_ok
  dbag.lookup(table, 1)
  |> should.equal(Ok([2, 3, 3]))
  dbag.take(table, 1)
  |> should.equal(Ok([2, 3, 3]))
  dbag.take(table, 1)
  |> should.equal(Error(bravo.Empty))
  dbag.lookup(table, 1)
  |> should.equal(Error(bravo.Empty))
  dbag.lookup(table, 3)
  |> should.equal(Ok([4]))
}

pub fn dbag_member_test() {
  let assert Ok(table) = dbag.new("dbag14", bravo.Public)
  use <- defer(fn() { dbag.delete(table) |> should.be_ok })
  dbag.insert_list(table, [#(1, 2), #(1, 3), #(1, 3), #(3, 4)])
  |> should.be_ok
  dbag.member(table, 1)
  |> should.equal(Ok(True))
  dbag.member(table, 2)
  |> should.equal(Ok(False))
  dbag.delete_key(table, 1)
  |> should.be_ok
  dbag.member(table, 1)
  |> should.equal(Ok(False))
  dbag.member(table, 3)
  |> should.equal(Ok(True))
  dbag.member(table, 4)
  |> should.equal(Ok(False))
}
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
