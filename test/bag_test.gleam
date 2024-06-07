import bravo
import bravo/bag
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option.{Some}
import gleeunit/should
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn bag_insert_lookup_delete_test() {
  let assert Ok(table) = bag.new("bag1", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([#(100, 200)])
  bag.lookup(table, 300)
  |> should.equal([#(300, 500)])
  bag.lookup(table, 600)
  |> should.equal([])
}

pub fn bag_multitype_test() {
  let assert Ok(table) = bag.new("bag2", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  bag.lookup(table, "a")
  |> should.equal([#("a", 1)])
  bag.lookup(table, "b")
  |> should.equal([#("b", 2)])
  bag.lookup(table, "c")
  |> should.equal([])
}

pub fn bag_large_test() {
  let assert Ok(table) = bag.new("bag3", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
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
  bag.lookup(table, 900)
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

pub fn bag_keypos_test() {
  let assert Ok(table) = bag.new("bag4", 2, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  bag.lookup(table, 200)
  |> should.equal([#(100, 200)])
  bag.lookup(table, 500)
  |> should.equal([#(300, 500)])
  bag.lookup(table, 100)
  |> should.equal([])
}

pub fn bag_bad_new_test() {
  let assert Ok(table) = bag.new("bag5", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.new("bag5", 1, bravo.Public)
  |> should.equal(Error(bravo.ErlangError("badarg")))
}

pub fn bag_bad_insert_test() {
  let assert Ok(table) = bag.new("bag6", 3, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("a", 1)])
  |> should.equal(False)
}

pub fn bag_multi_insert_test() {
  let assert Ok(table) = bag.new("bag7", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([#(100, 200)])
  bag.insert(table, [#(100, 300), #(100, 400), #(100, 400)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([#(100, 200), #(100, 300), #(100, 400)])
}

pub fn bag_large_multitype_test() {
  let assert Ok(table) = bag.new("bag8", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
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
  bag.lookup(table, "String")
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

pub fn bag_delete_test() {
  let assert Ok(table) = bag.new("bag9", 1, bravo.Public)
  bag.delete(table)
  |> should.equal(True)
  let assert Ok(table) = bag.new("bag9", 1, bravo.Public)
  bag.delete(table)
  |> should.equal(True)
  bag.delete(table)
  |> should.equal(False)
}

pub fn bag_singleton_test() {
  let assert Ok(table) = bag.new("bag10", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(1), #(2)])
  |> should.equal(True)
  bag.lookup(table, 1)
  |> should.equal([#(1)])
  bag.lookup(table, 2)
  |> should.equal([#(2)])
}

pub fn bag_nontuple_test() {
  let assert Ok(table) = bag.new("bag11", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [5])
  |> should.equal(True)
  bag.lookup(table, 5)
  |> should.equal([5])
}

pub fn bag_nontuple_record_test() {
  let assert Ok(table) = bag.new("bag12", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [Ok(5)])
  |> should.equal(True)
  bag.lookup(table, Ok(5))
  |> should.equal([Ok(5)])
}

type Multirecord {
  A(Int)
  B(Int, Int)
  C
}

pub fn bag_nontuple_multirecord_test() {
  let assert Ok(table) = bag.new("bag13", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [A(1), B(2, 3), C])
  |> should.equal(True)
  bag.lookup(table, A(1))
  |> should.equal([A(1)])
  bag.lookup(table, B(2, 3))
  |> should.equal([B(2, 3)])
  bag.lookup(table, C)
  |> should.equal([C])
}

pub fn bag_delete_key_test() {
  let assert Ok(table) = bag.new("bag14", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
  |> should.equal(True)
  bag.lookup(table, "Bye")
  |> should.equal([#("Bye", "World"), #("Bye", "Bye")])
  bag.delete_key(table, "Bye")
  bag.lookup(table, "Bye")
  |> should.equal([])
  bag.lookup(table, "Hello")
  |> should.equal([#("Hello", "World")])
}

pub fn bag_delete_all_objects_test() {
  let assert Ok(table) = bag.new("bag15", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
  |> should.equal(True)
  bag.delete_all_objects(table)
  bag.lookup(table, "Hello")
  |> should.equal([])
  bag.lookup(table, "Bye")
  |> should.equal([])
}

pub fn bag_delete_object_test() {
  let assert Ok(table) = bag.new("bag16", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("Hello", "World"), #("Bye", "World"), #("Bye", "Bye")])
  |> should.equal(True)
  bag.delete_object(table, #("Bye", "Bye"))
  bag.lookup(table, "Hello")
  |> should.equal([#("Hello", "World")])
  bag.lookup(table, "Bye")
  |> should.equal([#("Bye", "World")])
}

pub fn bag_tab2file_test() {
  let assert Ok(table) = bag.new("bag17", 2, bravo.Public)
  bag.insert(table, [#("Hello", "World")])
  |> should.equal(True)
  bag.tab2file(table, "bag17", True, True, True)
  |> should.equal(Ok(Nil))
  bag.delete(table)
  let assert Ok(new_table) =
    bag.file2tab("bag17", True, dynamic.tuple2(dynamic.string, dynamic.string))
  bag.lookup(new_table, "World")
  |> should.equal([#("Hello", "World")])
  bag.delete(new_table)
  bag.file2tab("bag17", True, dynamic.tuple2(dynamic.int, dynamic.int))
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("bag17")
  |> should.equal(Ok(Nil))
}

pub fn bag_tab2list_test() {
  let assert Ok(table) = bag.new("bag18", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  let objects = bag.tab2list(table)
  list.contains(objects, #("Hello", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "Bye"))
  |> should.equal(False)
}

pub fn bag_tab2list_orderedness_test() {
  let assert Ok(table) = bag.new("bag19", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
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
  bag.tab2list(table)
  |> should.not_equal([
    #("A"),
    #("B"),
    #("C"),
    #("DA"),
    #("Da"),
    #("Db"),
    #("F"),
    #("Q"),
    #("R"),
    #("S"),
    #("Z"),
    #("a"),
  ])
}

pub fn bag_empty_insert_test() {
  let assert Ok(table) = bag.new("bag20", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [])
  |> should.equal(False)
}

pub fn bag_dynamic_test() {
  let assert Ok(table) = bag.new("bag21", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
    dynamic.from(#("Hello", "World")),
    dynamic.from(#("Hello", "my", "friend!")),
    dynamic.from(#(1, 2, 3)),
  ])
  |> should.equal(True)
  let list = bag.lookup(table, "Hello")
  list.contains(list, dynamic.from(#("Hello", "World")))
  |> should.equal(True)
  list.contains(list, dynamic.from(#("Hello", "my", "friend!")))
  |> should.equal(True)
  bag.lookup(table, 1)
  |> should.equal([dynamic.from(#(1, 2, 3))])
}

pub fn bag_insert_new_test() {
  let assert Ok(table) = bag.new("bag22", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert_new(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  bag.insert_new(table, [#(1, 3), #(2, 4)])
  |> should.equal(False)
  bag.lookup(table, 1)
  |> should.equal([#(1, 2)])
  bag.lookup(table, 2)
  |> should.equal([])
}

pub fn bag_take_test() {
  let assert Ok(table) = bag.new("bag23", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  bag.lookup(table, 1)
  |> should.equal([#(1, 2)])
  bag.take(table, 1)
  |> should.equal([#(1, 2)])
  bag.take(table, 1)
  |> should.equal([])
  bag.lookup(table, 1)
  |> should.equal([])
}

pub fn bag_member_test() {
  let assert Ok(table) = bag.new("bag24", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  bag.member(table, 1)
  |> should.equal(True)
  bag.member(table, 2)
  |> should.equal(False)
  bag.delete_key(table, 1)
  bag.member(table, 1)
  |> should.equal(False)
}

pub fn bag_singleton_member_test() {
  let assert Ok(table) = bag.new("bag25", 1, bravo.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [1, 3])
  |> should.equal(True)
  bag.member(table, 1)
  |> should.equal(True)
  bag.member(table, 2)
  |> should.equal(False)
  bag.delete_key(table, 1)
  bag.member(table, 1)
  |> should.equal(False)
}

pub fn bag_tab2file_singleton_test() {
  let assert Ok(table) = bag.new("bag26", 1, bravo.Public)
  bag.insert(table, ["Hello"])
  |> should.equal(True)
  bag.tab2file(table, "bag26", True, True, True)
  |> should.equal(Ok(Nil))
  bag.delete(table)
  let assert Ok(new_table) = bag.file2tab("bag26", True, dynamic.string)
  bag.lookup(new_table, "Hello")
  |> should.equal(["Hello"])
  bag.delete(new_table)
  bag.file2tab("bag26", True, dynamic.tuple2(dynamic.int, dynamic.int))
  |> should.equal(Error(bravo.DecodeFailure))

  let assert Ok(newer_table) = bag.new("bag26", 1, bravo.Public)
  bag.insert(newer_table, [#("Hello")])
  |> should.equal(True)
  bag.tab2file(newer_table, "bag26", True, True, True)
  |> should.equal(Ok(Nil))
  bag.delete(newer_table)
  let assert Ok(newest_table) = bag.file2tab("bag26", True, dynamic.string)
  bag.lookup(newest_table, "Hello")
  |> should.equal(["Hello"])
  bag.delete(newest_table)

  simplifile.delete("bag26")
  |> should.equal(Ok(Nil))
}

pub fn bag_tab2file_singleton_record_test() {
  let assert Ok(table) = bag.new("bag27", 1, bravo.Public)
  bag.insert(table, [Ok("Hello"), Error("World")])
  |> should.equal(True)
  bag.tab2file(table, "bag27", True, True, True)
  |> should.equal(Ok(Nil))
  bag.delete(table)
  let assert Ok(new_table) =
    bag.file2tab("bag27", True, dynamic.result(dynamic.string, dynamic.string))
  bag.lookup(new_table, Ok("Hello"))
  |> should.equal([Ok("Hello")])
  bag.lookup(new_table, Error("World"))
  |> should.equal([Error("World")])
  bag.delete(new_table)
  bag.file2tab("bag27", True, dynamic.tuple2(dynamic.int, dynamic.int))
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("bag27")
  |> should.equal(Ok(Nil))
}
