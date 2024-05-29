import bravo/dbag
import bravo/error
import bravo/etc
import gleam/dict
import gleam/option.{Some}
import gleeunit/should

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
  |> should.equal(Error(Some(error.Badarg)))
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
