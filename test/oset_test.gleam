import bravo/error
import bravo/etc
import bravo/oset
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

pub fn oset_insert_lookup_delete_test() {
  let assert Ok(table) = oset.new("oset1", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  oset.lookup(table, 300)
  |> should.equal(Some(#(300, 500)))
  oset.lookup(table, 600)
  |> should.equal(None)
}

pub fn oset_multitype_test() {
  let assert Ok(table) = oset.new("oset2", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  oset.lookup(table, "a")
  |> should.equal(Some(#("a", 1)))
  oset.lookup(table, "b")
  |> should.equal(Some(#("b", 2)))
  oset.lookup(table, "c")
  |> should.equal(None)
}

pub fn oset_large_test() {
  let assert Ok(table) = oset.new("oset3", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
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
  oset.lookup(table, 900)
  |> should.equal(
    Some(#(
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
    )),
  )
}

pub fn oset_keypos_test() {
  let assert Ok(table) = oset.new("oset4", 2, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  oset.lookup(table, 200)
  |> should.equal(Some(#(100, 200)))
  oset.lookup(table, 500)
  |> should.equal(Some(#(300, 500)))
  oset.lookup(table, 100)
  |> should.equal(None)
}

pub fn oset_bad_new_test() {
  let assert Ok(table) = oset.new("oset5", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.new("oset5", 1, etc.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn oset_bad_insert_test() {
  let assert Ok(table) = oset.new("oset6", 3, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1)])
  |> should.equal(False)
}

pub fn oset_multi_insert_test() {
  let assert Ok(table) = oset.new("oset7", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  oset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 400)))
}

pub fn oset_large_multitype_test() {
  let assert Ok(table) = oset.new("oset8", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
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
  oset.lookup(table, "String")
  |> should.equal(
    Some(#(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Some(50),
    )),
  )
}

pub fn oset_delete_test() {
  let assert Ok(table) = oset.new("oset9", 1, etc.Public)
  oset.delete(table)
  |> should.equal(True)
  let assert Ok(table) = oset.new("oset9", 1, etc.Public)
  oset.delete(table)
  |> should.equal(True)
  oset.delete(table)
  |> should.equal(False)
}

pub fn oset_singleton_test() {
  let assert Ok(table) = oset.new("oset10", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(1), #(2)])
  |> should.equal(True)
  oset.lookup(table, 1)
  |> should.equal(Some(#(1)))
  oset.lookup(table, 2)
  |> should.equal(Some(#(2)))
}

pub fn oset_nontuple_test() {
  let assert Ok(table) = oset.new("oset11", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [5])
  |> should.equal(True)
  oset.lookup(table, 5)
  |> should.equal(Some(5))
}

pub fn oset_nontuple_record_test() {
  let assert Ok(table) = oset.new("oset12", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [Ok(5)])
  |> should.equal(True)
  oset.lookup(table, Ok(5))
  |> should.equal(Some(Ok(5)))
}

type Multirecord {
  A(Int)
  B(Int, Int)
  C
}

pub fn oset_nontuple_multirecord_test() {
  let assert Ok(table) = oset.new("oset13", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [A(1), B(2, 3), C])
  |> should.equal(True)
  oset.lookup(table, A(1))
  |> should.equal(Some(A(1)))
  oset.lookup(table, B(2, 3))
  |> should.equal(Some(B(2, 3)))
  oset.lookup(table, C)
  |> should.equal(Some(C))
}

pub fn oset_delete_key_test() {
  let assert Ok(table) = oset.new("oset14", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  oset.lookup(table, "Bye")
  |> should.equal(Some(#("Bye", "World")))
  oset.delete_key(table, "Bye")
  oset.lookup(table, "Bye")
  |> should.equal(None)
  oset.lookup(table, "Hello")
  |> should.equal(Some(#("Hello", "World")))
}

pub fn oset_delete_all_objects_test() {
  let assert Ok(table) = oset.new("oset15", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  oset.delete_all_objects(table)
  oset.lookup(table, "Hello")
  |> should.equal(None)
  oset.lookup(table, "Bye")
  |> should.equal(None)
}

pub fn oset_delete_object_test() {
  let assert Ok(table) = oset.new("oset16", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  oset.delete_object(table, #("Bye", "World"))
  oset.lookup(table, "Hello")
  |> should.equal(Some(#("Hello", "World")))
  oset.lookup(table, "Bye")
  |> should.equal(None)
}

pub fn oset_tab2file_test() {
  let assert Ok(table) = oset.new("oset17", 2, etc.Public)
  oset.insert(table, [#("Hello", "World")])
  |> should.equal(True)
  oset.tab2file(table, "oset17", True, True, True)
  |> should.equal(True)
  oset.delete(table)
  let assert Some(new_table) =
    oset.file2tab(
      "oset17",
      True,
      dynamic.tuple2(dynamic.string, dynamic.string),
    )
  oset.lookup(new_table, "World")
  |> should.equal(Some(#("Hello", "World")))
  oset.delete(new_table)
  oset.file2tab("oset17", True, dynamic.tuple2(dynamic.int, dynamic.int))
  |> should.equal(None)
  simplifile.delete("oset17")
  |> should.equal(Ok(Nil))
}

pub fn oset_tab2list_test() {
  let assert Ok(table) = oset.new("oset18", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.equal(True)
  let objects = oset.tab2list(table)
  list.contains(objects, #("Hello", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "World"))
  |> should.equal(True)
  list.contains(objects, #("Bye", "Bye"))
  |> should.equal(False)
}

pub fn oset_tab2list_orderedness_test() {
  let assert Ok(table) = oset.new("oset19", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
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
  oset.tab2list(table)
  |> should.equal([
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

pub fn oset_empty_insert_test() {
  let assert Ok(table) = oset.new("oset20", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [])
  |> should.equal(False)
}

pub fn oset_dynamic_test() {
  let assert Ok(table) = oset.new("oset21", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
    dynamic.from(#("Hello", "World")),
    dynamic.from(#(1, 2, 3)),
  ])
  |> should.equal(True)
  oset.lookup(table, "Hello")
  |> should.equal(Some(dynamic.from(#("Hello", "World"))))
  oset.lookup(table, 1)
  |> should.equal(Some(dynamic.from(#(1, 2, 3))))
}

pub fn oset_insert_new_test() {
  let assert Ok(table) = oset.new("oset22", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert_new(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  oset.insert_new(table, [#(1, 3), #(2, 4)])
  |> should.equal(False)
  oset.lookup(table, 1)
  |> should.equal(Some(#(1, 2)))
  oset.lookup(table, 2)
  |> should.equal(None)
}

pub fn oset_take_test() {
  let assert Ok(table) = oset.new("oset23", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  oset.lookup(table, 1)
  |> should.equal(Some(#(1, 2)))
  oset.take(table, 1)
  |> should.equal(Some(#(1, 2)))
  oset.take(table, 1)
  |> should.equal(None)
  oset.lookup(table, 1)
  |> should.equal(None)
}

pub fn oset_member_test() {
  let assert Ok(table) = oset.new("oset24", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(1, 2), #(3, 4)])
  |> should.equal(True)
  oset.member(table, 1)
  |> should.equal(True)
  oset.member(table, 2)
  |> should.equal(False)
  oset.delete_key(table, 1)
  oset.member(table, 1)
  |> should.equal(False)
}

pub fn oset_singleton_member_test() {
  let assert Ok(table) = oset.new("oset25", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [1, 3])
  |> should.equal(True)
  oset.member(table, 1)
  |> should.equal(True)
  oset.member(table, 2)
  |> should.equal(False)
  oset.delete_key(table, 1)
  oset.member(table, 1)
  |> should.equal(False)
}
