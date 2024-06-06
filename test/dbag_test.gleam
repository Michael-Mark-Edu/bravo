import bravo
import bravo/dbag
import bravo/error
import bravo/etc
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn dbag_insert_lookup_delete_test() {
  let assert Ok(table) = dbag.new("dbag1", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([#(100, 200)])
  dbag.lookup(table, 300)
  |> should.equal([#(300, 500)])
  dbag.lookup(table, 600)
  |> should.equal([])
}

pub fn dbag_multitype_test() {
  let assert Ok(table) = dbag.new("dbag2", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  dbag.lookup(table, "a")
  |> should.equal([#("a", 1)])
  dbag.lookup(table, "b")
  |> should.equal([#("b", 2)])
  dbag.lookup(table, "c")
  |> should.equal([])
}

pub fn dbag_large_test() {
  let assert Ok(table) = dbag.new("dbag3", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
    #(
      900,
      800,
      700,
      600,
      500,
      400,
      300,
      200,
      100,
      0,
      -100,
      -200,
      -300,
      -400,
      -500,
      -600,
      -700,
      -800,
      -900,
    ),
  ])
  |> should.equal(True)
  dbag.lookup(table, 900)
  |> should.equal([
    #(
      900,
      800,
      700,
      600,
      500,
      400,
      300,
      200,
      100,
      0,
      -100,
      -200,
      -300,
      -400,
      -500,
      -600,
      -700,
      -800,
      -900,
    ),
  ])
}

pub fn dbag_keypos_test() {
  let assert Ok(table) = dbag.new("dbag4", 2, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  dbag.lookup(table, 200)
  |> should.equal([#(100, 200)])
  dbag.lookup(table, 500)
  |> should.equal([#(300, 500)])
  dbag.lookup(table, 100)
  |> should.equal([])
}

pub fn dbag_bad_new_test() {
  let assert Ok(table) = dbag.new("dbag5", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.new("dbag5", 1, etc.Public)
  |> should.equal(Error(bravo.ErlangError("badarg")))
}

pub fn dbag_bad_insert_test() {
  let assert Ok(table) = dbag.new("dbag6", 3, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("a", 1)])
  |> should.equal(False)
}

pub fn dbag_multi_insert_test() {
  let assert Ok(table) = dbag.new("dbag7", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([#(100, 200)])
  dbag.insert(table, [#(100, 300), #(100, 400), #(100, 400)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([#(100, 200), #(100, 300), #(100, 400), #(100, 400)])
}

pub fn dbag_large_multitype_test() {
  let assert Ok(table) = dbag.new("dbag8", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
    #(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Some(50),
    ),
  ])
  |> should.equal(True)
  dbag.lookup(table, "String")
  |> should.equal([
    #(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Some(50),
    ),
  ])
}

pub fn dbag_delete_test() {
  let assert Ok(table) = dbag.new("dbag9", 1, etc.Public)
  dbag.delete(table)
  |> should.equal(True)
  let assert Ok(table) = dbag.new("dbag9", 1, etc.Public)
  dbag.delete(table)
  |> should.equal(True)
  dbag.delete(table)
  |> should.equal(False)
}

pub fn dbag_singleton_test() {
  let assert Ok(table) = dbag.new("dbag10", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(1), #(2)])
  |> should.equal(True)
  dbag.lookup(table, 1)
  |> should.equal([#(1)])
  dbag.lookup(table, 2)
  |> should.equal([#(2)])
}

pub fn dbag_nontuple_test() {
  let assert Ok(table) = dbag.new("dbag11", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [5])
  |> should.equal(True)
  dbag.lookup(table, 5)
  |> should.equal([5])
}

pub fn dbag_nontuple_record_test() {
  let assert Ok(table) = dbag.new("dbag12", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [Ok(5)])
  |> should.equal(True)
  dbag.lookup(table, Ok(5))
  |> should.equal([Ok(5)])
}

type Multirecord {
  A(Int)
  B(Int, Int)
  C
}

pub fn dbag_nontuple_multirecord_test() {
  let assert Ok(table) = dbag.new("dbag14", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [A(1), B(2, 3), C])
  |> should.equal(True)
  dbag.lookup(table, A(1))
  |> should.equal([A(1)])
  dbag.lookup(table, B(2, 3))
  |> should.equal([B(2, 3)])
  dbag.lookup(table, C)
  |> should.equal([C])
}

pub fn dbag_delete_key_test() {
  let assert Ok(table) = dbag.new("dbag14", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
  |> should.equal(True)
  dbag.lookup(table, "Bye")
  |> should.equal([#("Bye", "World"), #("Bye", "Bye")])
  dbag.delete_key(table, "Bye")
  dbag.lookup(table, "Bye")
  |> should.equal([])
  dbag.lookup(table, "Hello")
  |> should.equal([#("Hello", "World")])
}

pub fn dbag_delete_all_objects_test() {
  let assert Ok(table) = dbag.new("dbag15", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
  |> should.equal(True)
  dbag.delete_all_objects(table)
  dbag.lookup(table, "Hello")
  |> should.equal([])
  dbag.lookup(table, "Bye")
  |> should.equal([])
}

pub fn dbag_delete_object_test() {
  let assert Ok(table) = dbag.new("dbag16", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
    #("Hello", "World"),
    #("Bye", "World"),
    #("Bye", "Bye"),
    #("Bye", "Bye"),
  ])
  |> should.equal(True)
  dbag.delete_object(table, #("Bye", "Bye"))
  dbag.lookup(table, "Hello")
  |> should.equal([#("Hello", "World")])
  dbag.lookup(table, "Bye")
  |> should.equal([#("Bye", "World")])
}

pub fn dbag_tab2file_test() {
  let assert Ok(table) = dbag.new("dbag17", 2, etc.Public)
  dbag.insert(table, [#("Hello", "World")])
  |> should.equal(True)
  dbag.tab2file(table, "dbag17", True, True, True)
  |> should.equal(True)
  dbag.delete(table)
  let assert Some(new_table) =
    dbag.file2tab(
      "dbag17",
      True,
      dynamic.tuple2(dynamic.string, dynamic.string),
    )
  dbag.lookup(new_table, "World")
  |> should.equal([#("Hello", "World")])
  dbag.delete(new_table)
  dbag.file2tab("dbag17", True, dynamic.tuple2(dynamic.int, dynamic.int))
  |> should.equal(None)
  simplifile.delete("dbag17")
  |> should.equal(Ok(Nil))
}

pub fn dbag_tab2list_test() {
  let assert Ok(table) = dbag.new("dbag18", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  let objects = dbag.tab2list(table)
  list.contains(objects, #("Hello", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "Bye"))
  |> should.equal(False)
}

pub fn dbag_tab2list_orderedness_test() {
  let assert Ok(table) = dbag.new("dbag19", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
    #("A"),
    #("Q"),
    #("C"),
    #("R"),
    #("Z"),
    #("B"),
    #("S"),
    #("F"),
    #("Da"),
    #("DA"),
    #("Db"),
    #("a"),
  ])
  |> should.equal(True)
  dbag.tab2list(table)
  |> should.not_equal([
    #("A"),
    #("Q"),
    #("C"),
    #("R"),
    #("Z"),
    #("B"),
    #("S"),
    #("F"),
    #("Da"),
    #("DA"),
    #("Db"),
    #("a"),
  ])
}

pub fn dbag_empty_insert_test() {
  let assert Ok(table) = dbag.new("dbag20", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [])
  |> should.equal(False)
}

pub fn dbag_dynamic_test() {
  let assert Ok(table) = dbag.new("dbag21", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
    dynamic.from(#("Hello", "World")),
    dynamic.from(#("Hello", "my", "friend!")),
    dynamic.from(#("Hello", "World")),
    dynamic.from(#(1, 2, 3)),
  ])
  |> should.equal(True)
  let list = dbag.lookup(table, "Hello")
  list.contains(list, dynamic.from(#("Hello", "World")))
  |> should.equal(True)
  let assert Ok(#(_, newlist)) = {
    use a <- list.pop(list)
    let fun = dynamic.tuple2(dynamic.string, dynamic.string)
    let assert Ok(res) = fun(a)
    res == #("Hello", "World")
  }
  list.contains(newlist, dynamic.from(#("Hello", "World")))
  |> should.equal(True)
  list.contains(newlist, dynamic.from(#("Hello", "my", "friend!")))
  |> should.equal(True)
  dbag.lookup(table, 1)
  |> should.equal([dynamic.from(#(1, 2, 3))])
}

pub fn dbag_insert_new_test() {
  let assert Ok(table) = dbag.new("dbag22", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert_new(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  dbag.insert_new(table, [#(1, 3), #(2, 4)])
  |> should.equal(False)
  dbag.lookup(table, 1)
  |> should.equal([#(1, 2)])
  dbag.lookup(table, 2)
  |> should.equal([])
}

pub fn dbag_take_test() {
  let assert Ok(table) = dbag.new("dbag23", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  dbag.lookup(table, 1)
  |> should.equal([#(1, 2)])
  dbag.take(table, 1)
  |> should.equal([#(1, 2)])
  dbag.take(table, 1)
  |> should.equal([])
  dbag.lookup(table, 1)
  |> should.equal([])
}

pub fn dbag_member_test() {
  let assert Ok(table) = dbag.new("dbag24", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  dbag.member(table, 1)
  |> should.equal(True)
  dbag.member(table, 2)
  |> should.equal(False)
  dbag.delete_key(table, 1)
  dbag.member(table, 1)
  |> should.equal(False)
}

pub fn dbag_singleton_member_test() {
  let assert Ok(table) = dbag.new("dbag25", 1, etc.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [1, 3])
  |> should.equal(True)
  dbag.member(table, 1)
  |> should.equal(True)
  dbag.member(table, 2)
  |> should.equal(False)
  dbag.delete_key(table, 1)
  dbag.member(table, 1)
  |> should.equal(False)
}
