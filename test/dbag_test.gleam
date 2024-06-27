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
//
// pub fn ddbag_insert_new_test() {
//   let assert Ok(table) = ddbag.new("ddbag22", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   ddbag.insert_new(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   ddbag.insert_new(table, [#(1, 3), #(2, 4)])
//   |> should.equal(Error(bravo.KeyAlreadyPresent))
//   ddbag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   ddbag.lookup(table, 2)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn ddbag_take_test() {
//   let assert Ok(table) = ddbag.new("ddbag23", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   ddbag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   ddbag.lookup(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   ddbag.take(table, 1)
//   |> should.equal(Ok([#(1, 2)]))
//   ddbag.take(table, 1)
//   |> should.equal(Error(bravo.Empty))
//   ddbag.lookup(table, 1)
//   |> should.equal(Error(bravo.Empty))
// }
//
// pub fn ddbag_member_test() {
//   let assert Ok(table) = ddbag.new("ddbag24", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   ddbag.insert(table, [#(1, 2), #(3, 4)])
//   |> should.equal(Ok(Nil))
//   ddbag.member(table, 1)
//   |> should.equal(True)
//   ddbag.member(table, 2)
//   |> should.equal(False)
//   ddbag.delete_key(table, 1)
//   ddbag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn ddbag_singleton_member_test() {
//   let assert Ok(table) = ddbag.new("ddbag25", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   ddbag.insert(table, [1, 3])
//   |> should.equal(Ok(Nil))
//   ddbag.member(table, 1)
//   |> should.equal(True)
//   ddbag.member(table, 2)
//   |> should.equal(False)
//   ddbag.delete_key(table, 1)
//   ddbag.member(table, 1)
//   |> should.equal(False)
// }
//
// pub fn ddbag_tab2file_singleton_test() {
//   let assert Ok(table) = ddbag.new("ddbag26", 1, bravo.Public)
//   ddbag.insert(table, ["Hello"])
//   |> should.equal(Ok(Nil))
//   ddbag.tab2file(table, "ddbag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   ddbag.delete(table)
//   let assert Ok(new_table) = ddbag.file2tab("ddbag26", True, dynamic.string)
//   ddbag.lookup(new_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   ddbag.delete(new_table)
//   ddbag.file2tab("ddbag26", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//
//   let assert Ok(newer_table) = ddbag.new("ddbag26", 1, bravo.Public)
//   ddbag.insert(newer_table, [#("Hello")])
//   |> should.equal(Ok(Nil))
//   ddbag.tab2file(newer_table, "ddbag26", True, True, True)
//   |> should.equal(Ok(Nil))
//   ddbag.delete(newer_table)
//   let assert Ok(newest_table) = ddbag.file2tab("ddbag26", True, dynamic.string)
//   ddbag.lookup(newest_table, "Hello")
//   |> should.equal(Ok(["Hello"]))
//   ddbag.delete(newest_table)
//
//   simplifile.delete("ddbag26")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn ddbag_tab2file_singleton_record_test() {
//   let assert Ok(table) = ddbag.new("ddbag27", 1, bravo.Public)
//   ddbag.insert(table, [Ok("Hello"), Error("World")])
//   |> should.equal(Ok(Nil))
//   ddbag.tab2file(table, "ddbag27", True, True, True)
//   |> should.equal(Ok(Nil))
//   ddbag.delete(table)
//   let assert Ok(new_table) =
//     ddbag.file2tab(
//       "ddbag27",
//       True,
//       dynamic.result(dynamic.string, dynamic.string),
//     )
//   ddbag.lookup(new_table, Ok("Hello"))
//   |> should.equal(Ok([Ok("Hello")]))
//   ddbag.lookup(new_table, Error("World"))
//   |> should.equal(Ok([Error("World")]))
//   ddbag.delete(new_table)
//   ddbag.file2tab("ddbag27", True, dynamic.tuple2(dynamic.int, dynamic.int))
//   |> should.equal(Error(bravo.DecodeFailure))
//   simplifile.delete("ddbag27")
//   |> should.equal(Ok(Nil))
// }
//
// pub fn ddbag_fn_test() {
//   let assert Ok(table) = ddbag.new("ddbag28", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   let dataset = [
//     "A", "Q", "C", "R", "Z", "Z", "B", "S", "F", "Da", "DA", "Db", "a",
//   ]
//   ddbag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> ddbag.first
//   let assert Ok(a) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(b) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(c) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(d) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(e) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(f) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(g) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(h) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(i) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(j) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(k) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.next(key)
//   let assert Ok(l) = ddbag.lookup(table, key)
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
//   table |> ddbag.next(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// pub fn ddbag_lp_test() {
//   let assert Ok(table) = ddbag.new("ddbag29", 1, bravo.Public)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   let dataset = [
//     "A", "Q", "C", "R", "Z", "Z", "B", "S", "F", "Da", "DA", "Db", "a",
//   ]
//   ddbag.insert(table, dataset)
//   |> should.equal(Ok(Nil))
//   let assert Ok(key) = table |> ddbag.last
//   let assert Ok(a) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(b) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(c) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(d) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(e) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(f) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(g) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(h) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(i) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(j) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(k) = ddbag.lookup(table, key)
//   let assert Ok(key) = table |> ddbag.prev(key)
//   let assert Ok(l) = ddbag.lookup(table, key)
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
//   table |> ddbag.prev(l) |> should.equal(Error(Nil))
//   Error(Nil)
// }
//
// // TODO: Replace the internal binding calls here with actual functions
// pub fn ddbag_async_access_test() {
//   let assert Ok(table) = ddbag.new("ddbag30", 1, bravo.Private)
//   use <- defer(fn() { ddbag.delete(table) |> should.equal(True) })
//   ddbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   let assert Ok(ref) =
//     "ddbag30"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Hello")
//   |> should.equal(Ok([#("Hello", "World")]))
//   let task = {
//     use <- task.async
//     let assert Ok(ref) =
//       "ddbag30"
//       |> atom.create_from_string
//       |> bindings.try_whereis
//     bindings.try_lookup(ref, "Hello")
//     |> should.equal(Error(bravo.AccessDenied))
//   }
//   task.await_forever(task)
// }
//
// pub fn ddbag_async_protected_test() {
//   let assert Ok(actor) = {
//     use _, _ <- actor.start(option.None)
//     let assert Ok(table) = ddbag.new("ddbag30a", 1, bravo.Protected)
//     ddbag.insert(table, [#("Goodbye", "World")])
//     |> should.equal(Ok(Nil))
//     actor.Continue(option.Some(table), option.None)
//   }
//   actor.send(actor, Nil)
//   process.sleep(100)
//   let assert Ok(ref) =
//     "ddbag30a"
//     |> atom.create_from_string
//     |> bindings.try_whereis
//   bindings.try_lookup(ref, "Goodbye")
//   |> should.equal(Ok([#("Goodbye", "World")]))
//   bindings.try_insert(ref, 1, [#("Hello", "Again")])
//   |> should.equal(Error(bravo.AccessDenied))
// }
//
// pub fn ddbag_recreation_test() {
//   let assert Ok(table) = ddbag.new("ddbag31", 1, bravo.Private)
//   ddbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Ok(Nil))
//   ddbag.delete(table)
//   let assert Ok(_table2) = ddbag.new("ddbag31", 1, bravo.Private)
//   ddbag.insert(table, [#("Hello", "World")])
//   |> should.equal(Error(bravo.TableDoesNotExist))
// }
